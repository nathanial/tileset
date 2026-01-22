/-
  Tileset/Manager.lean - Reactive Tile Manager

  The TileManager provides a reactive interface for tile loading:
  - Each tile request returns a Dynamic that updates when state changes
  - Fetching is done via wisp HTTP client with background tasks
  - Two-tier caching: memory (Raster.Image) + disk (Cellar)
  - Fully reactive: no polling required, Dynamics update automatically
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Std.Sync.Channel
import Std.Sync.Mutex
import Tileset.Coord
import Tileset.Provider
import Tileset.State
import Tileset.Cache
import Tileset.Retry
import Raster
import Wisp
import Reactive

namespace Tileset

open Std (HashMap HashSet)
open Reactive (Dynamic Event)
open Reactive.Host (Spider SpiderM Dyn Evt)
open Retry (RetryConfig RetryState)

/-- Configuration for the TileManager -/
structure TileManagerConfig where
  /-- Tile provider (defines URL template, tile size, etc.) -/
  provider : TileProvider := TileProvider.default
  /-- Disk cache directory -/
  diskCacheDir : String := "./tile_cache"
  /-- Maximum disk cache size in bytes -/
  diskCacheMaxSize : Nat := 2000 * 1024 * 1024  -- 2 GB
  /-- Retry configuration -/
  retryConfig : RetryConfig := Retry.defaultRetryConfig
  /-- Unload configuration -/
  unloadConfig : UnloadConfig := defaultUnloadConfig
  /-- HTTP client timeout in milliseconds -/
  httpTimeout : UInt64 := 30000
  /-- Number of background workers for tile IO and decoding -/
  workerCount : Nat := 4
  deriving Repr, Inhabited

/-- Internal state for a tile's reactive binding -/
private structure TileBinding where
  /-- The Dynamic that consumers subscribe to -/
  dynamic : Dyn TileLoadState
  /-- Function to update the dynamic's value -/
  update : TileLoadState → IO Unit

private structure InFlight where
  cancel : Wisp.HTTP.CancelHandle
  cancelPromise : IO.Promise Unit

private inductive TileJobKind where
  | load
  | fetch
  | decode (bytes : ByteArray)
  deriving Inhabited

private structure TileJob where
  coord : TileCoord
  kind : TileJobKind
  priority : Int
  seq : Nat
  generation : Nat
  deriving Inhabited

private structure RequestInfo where
  priority : Int
  generation : Nat
  lastScheduled : Option Nat := none
  deriving Inhabited

private structure JobQueueState where
  heap : Array TileJob := #[]
  closed : Bool := false

private structure JobQueue where
  state : Std.Mutex JobQueueState
  signal : Std.CloseableChannel.Sync Unit

/-- The TileManager manages tile loading with reactive state -/
structure TileManager where
  private mk ::
  /-- Configuration -/
  config : TileManagerConfig
  /-- HTTP client -/
  httpClient : Wisp.HTTP.Client
  /-- Memory cache (internal state) -/
  memoryCacheRef : IO.Ref MemoryCache
  /-- Disk cache index -/
  diskCacheIndexRef : IO.Ref DiskCacheIndex
  /-- Per-tile reactive bindings -/
  bindingsRef : IO.Ref (HashMap TileCoord TileBinding)
  /-- In-flight network requests (for cancellation) -/
  inFlightRef : IO.Ref (HashMap TileCoord InFlight)
  /-- Canceled tiles (skip queued work until re-requested) -/
  canceledRef : IO.Ref (HashSet TileCoord)
  /-- Latest request info per tile (priority + generation). -/
  priorityRef : IO.Ref (HashMap TileCoord RequestInfo)
  /-- Current frame counter (for LRU tracking and retry timing) -/
  frameCounterRef : IO.Ref Nat
  /-- Background job queue for tile IO/decoding -/
  jobQueue : JobQueue
  /-- Monotonic sequence for stable FIFO ordering within priority -/
  jobSeqRef : IO.Ref Nat
  /-- Background worker tasks -/
  workerTasks : Array (Task (Except IO.Error Unit))
  /-- Shutdown flag (prevents new work from starting) -/
  isShutdownRef : IO.Ref Bool
  /-- Shutdown signal (used to abort in-flight fetch waits) -/
  shutdownPromise : IO.Promise Unit

namespace TileManager

/-- Internal: Create a TileBinding with a Dynamic -/
private def createBinding (initial : TileLoadState) : SpiderM TileBinding := do
  let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := TileLoadState)
  let dynamic ← Reactive.holdDyn initial event
  return { dynamic, update := trigger }

/-- Internal: Get or create a binding for a tile -/
private def getOrCreateBinding (mgr : TileManager) (coord : TileCoord)
    (initialState : TileLoadState) : SpiderM TileBinding := do
  let bindings ← SpiderM.liftIO mgr.bindingsRef.get
  match bindings[coord]? with
  | some binding => return binding
  | none =>
    let binding ← createBinding initialState
    SpiderM.liftIO <| mgr.bindingsRef.modify (·.insert coord binding)
    return binding

/-- Internal: Update a tile's state and notify subscribers -/
private def updateTileState (mgr : TileManager) (coord : TileCoord) (state : TileState)
    : IO Unit := do
  -- Update memory cache
  mgr.memoryCacheRef.modify (·.insert coord state)
  -- Update the binding if it exists
  let bindings ← mgr.bindingsRef.get
  if let some binding := bindings[coord]? then
    binding.update state.toLoadState

@[inline] private def jobHigher (a b : TileJob) : Bool :=
  if a.priority == b.priority then
    a.seq < b.seq
  else
    a.priority > b.priority

@[inline] private def parentIdx (i : Nat) : Nat := (i - 1) / 2
@[inline] private def leftChildIdx (i : Nat) : Nat := 2 * i + 1
@[inline] private def rightChildIdx (i : Nat) : Nat := 2 * i + 2

@[inline] private def swapJob (arr : Array TileJob) (i j : Nat) : Array TileJob :=
  let vi := arr[i]!
  let vj := arr[j]!
  (arr.set! i vj).set! j vi

partial def siftUpJob (arr : Array TileJob) (i : Nat) : Array TileJob :=
  if i == 0 then arr
  else
    let pi := parentIdx i
    if pi < arr.size && i < arr.size then
      if jobHigher arr[i]! arr[pi]! then
        siftUpJob (swapJob arr i pi) pi
      else
        arr
    else
      arr

partial def siftDownJob (arr : Array TileJob) (i : Nat) : Array TileJob :=
  let left := leftChildIdx i
  let right := rightChildIdx i
  let size := arr.size
  let best :=
    let b1 := if left < size && jobHigher arr[left]! arr[i]! then left else i
    if right < size && jobHigher arr[right]! arr[b1]! then right else b1
  if best != i then
    siftDownJob (swapJob arr i best) best
  else
    arr

private def heapInsert (arr : Array TileJob) (job : TileJob) : Array TileJob :=
  siftUpJob (arr.push job) arr.size

private def heapPop? (arr : Array TileJob) : (Option TileJob × Array TileJob) :=
  if arr.isEmpty then
    (none, arr)
  else if arr.size == 1 then
    (some arr[0]!, #[])
  else
    let root := arr[0]!
    let last := arr[arr.size - 1]!
    let arr' := arr.pop
    let arr'' := siftDownJob (arr'.set! 0 last) 0
    (some root, arr'')

private def jobQueueNew : IO JobQueue := do
  let state ← Std.Mutex.new ({ heap := #[], closed := false } : JobQueueState)
  let signal ← Std.CloseableChannel.Sync.new
  return { state, signal }

private def jobQueueClose (queue : JobQueue) : IO Unit := do
  let _ ← queue.state.atomically do
    let st ← get
    if st.closed then
      return ()
    else
      set { st with closed := true }
  try
    let _ ← Std.CloseableChannel.Sync.close queue.signal
    pure ()
  catch _ =>
    pure ()

private def jobQueuePush (queue : JobQueue) (job : TileJob) : IO Unit := do
  let shouldSignal ← queue.state.atomically do
    let st ← get
    if st.closed then
      return false
    else
      set { st with heap := heapInsert st.heap job }
      return true
  if shouldSignal then
    try
      let _ ← Std.CloseableChannel.Sync.send queue.signal ()
      pure ()
    catch _ =>
      pure ()

partial def jobQueueRecv (queue : JobQueue) : IO (Option TileJob) := do
  let signal? ← queue.signal.recv
  match signal? with
  | none => return none
  | some _ =>
      let job? ← queue.state.atomically do
        let st ← get
        let (job?, heap') := heapPop? st.heap
        set { st with heap := heap' }
        return job?
      match job? with
      | some job => return some job
      | none =>
          let closed ← queue.state.atomically do
            let st ← get
            return st.closed
          if closed then
            return none
          else
            jobQueueRecv queue

private def upsertRequestInfo (mgr : TileManager) (coord : TileCoord) (priority : Int)
    (generation : Nat) : IO RequestInfo := do
  mgr.priorityRef.modifyGet fun m =>
    let info := match m[coord]? with
      | some prev => { prev with priority := priority, generation := generation }
      | none => { priority, generation }
    (info, m.insert coord info)

private def getRequestInfo? (mgr : TileManager) (coord : TileCoord) : IO (Option RequestInfo) := do
  let priorities ← mgr.priorityRef.get
  pure priorities[coord]?

private def isPriorityStale (mgr : TileManager) (coord : TileCoord) (jobGeneration : Nat) : IO Bool := do
  let priorities ← mgr.priorityRef.get
  match priorities[coord]? with
  | none => pure true
  | some info => pure (info.generation != jobGeneration)

private def markScheduled (mgr : TileManager) (coord : TileCoord) (generation : Nat) : IO Unit := do
  mgr.priorityRef.modify fun m =>
    match m[coord]? with
    | none => m
    | some info => m.insert coord { info with lastScheduled := some generation }

private def enqueueJob (mgr : TileManager) (coord : TileCoord) (kind : TileJobKind)
    (priority : Int) (generation : Nat) : IO Unit := do
  if (← mgr.isShutdownRef.get) then
    pure ()
  else
    let seq ← mgr.jobSeqRef.modifyGet fun n => (n, n + 1)
    let job : TileJob := { coord, kind, priority, seq, generation }
    jobQueuePush mgr.jobQueue job

private def enqueueJobCurrent (mgr : TileManager) (coord : TileCoord) (kind : TileJobKind)
    (fallback : Int := 0) : IO Unit := do
  let info? ← getRequestInfo? mgr coord
  match info? with
  | none => pure ()
  | some info =>
      let priority := if info.priority == 0 then fallback else info.priority
      enqueueJob mgr coord kind priority info.generation

private def isCanceled (mgr : TileManager) (coord : TileCoord) : IO Bool := do
  let canceled ← mgr.canceledRef.get
  pure (canceled.contains coord)

private def cancelCoord (mgr : TileManager) (coord : TileCoord) : IO Unit := do
  mgr.canceledRef.modify (·.insert coord)
  mgr.priorityRef.modify (·.erase coord)
  let inflight ← mgr.inFlightRef.get
  if let some info := inflight[coord]? then
    try
      info.cancelPromise.resolve ()
    catch _ =>
      pure ()
    info.cancel.cancel
    mgr.inFlightRef.modify (·.erase coord)

private def cancelCoords (mgr : TileManager) (coords : List TileCoord) : IO Unit := do
  for coord in coords do
    cancelCoord mgr coord

private def decodeImage (coord : TileCoord) (bytes : ByteArray) : IO (Option Raster.Image) := do
  try
    let img ← Raster.Image.loadFromMemoryAs bytes .rgba
    pure (some img)
  catch e =>
    IO.eprintln s!"[Tileset] Decode error for {coord}: {e}"
    pure none


/-- Internal: Try to load from disk cache -/
private def tryDiskCache (mgr : TileManager) (coord : TileCoord) : IO (Option ByteArray) := do
  let diskConfig := DiskCacheConfig.fromProvider mgr.config.provider
    mgr.config.diskCacheDir mgr.config.diskCacheMaxSize
  DiskCache.read diskConfig coord

/-- Internal: Save to disk cache -/
private def saveToDiskCache (mgr : TileManager) (coord : TileCoord) (data : ByteArray)
    : IO Unit := do
  let diskConfig := DiskCacheConfig.fromProvider mgr.config.provider
    mgr.config.diskCacheDir mgr.config.diskCacheMaxSize
  -- Check if we need to evict
  let index ← mgr.diskCacheIndexRef.get
  let evictions := DiskCache.selectEvictions index data.size
  -- Delete evicted files
  for entry in evictions do
    try
      DiskCache.delete diskConfig entry.key
    catch _ => pure ()  -- Ignore deletion errors
  let index' := DiskCache.removeEntries index evictions
  -- Write new file
  try
    DiskCache.write diskConfig coord data
    let now ← Cellar.nowMs
    let entry : DiskCacheEntry := {
      key := coord
      filePath := tilePath diskConfig coord
      sizeBytes := data.size
      lastAccessTime := now
    }
    let index'' := DiskCache.addEntry index' entry
    mgr.diskCacheIndexRef.set index''
  catch _ => pure ()  -- Ignore write errors

-- Sleep in small increments, aborting early if the manager is shutting down.
private def sleepWithShutdown (mgr : TileManager) (delayMs : Nat) (stepMs : Nat := 100) : IO Bool := do
  let stepMs := if stepMs == 0 then 1 else stepMs
  let steps := (delayMs + stepMs - 1) / stepMs
  let mut remaining := delayMs
  for _ in [:steps] do
    if (← mgr.isShutdownRef.get) then
      return false
    let step := Nat.min stepMs remaining
    IO.sleep step.toUInt32
    remaining := remaining - step
  pure true

mutual
/-- Internal: Schedule a retry check after the backoff delay -/
partial def scheduleRetryCheck (mgr : TileManager) (coord : TileCoord) (rs : RetryState)
    : IO Unit := do
  if (← mgr.isShutdownRef.get) then
    pure ()
  else
  if (← mgr.isCanceled coord) then
    pure ()
  else
  if rs.isExhausted mgr.config.retryConfig then
    -- No more retries - mark as exhausted
    let state := TileState.exhausted rs
    mgr.updateTileState coord state
  else
    -- Schedule retry after backoff delay
    let delayMs := rs.backoffDelay mgr.config.retryConfig * 16  -- ~16ms per frame at 60fps
    let retryAction : IO Unit := do
      let shouldContinue ← sleepWithShutdown mgr delayMs
      if shouldContinue then
        -- Check if tile is still in failed state (not evicted or replaced)
        let cache ← mgr.memoryCacheRef.get
        match cache.get coord with
        | some (.failed currentRs) =>
          if currentRs == rs then  -- Still the same failure
            enqueueJobCurrent mgr coord .fetch
          else
            pure ()
        | _ => pure ()  -- Tile state changed, skip retry
    let _ ← retryAction.asTask
    pure ()

/-- Internal: Handle fetch completion (called from background task) -/
partial def handleFetchComplete (mgr : TileManager) (coord : TileCoord)
    (result : Wisp.WispResult Wisp.Response) (wasRetry : Bool) : IO Unit := do
  if (← mgr.isShutdownRef.get) then
    pure ()
  else
  if (← mgr.isCanceled coord) then
    pure ()
  else
  let frame ← mgr.frameCounterRef.get
  match result with
  | .ok response =>
    if response.status >= 200 && response.status < 300 then
      -- Success - decode the image
      try
        let img ← Raster.Image.loadFromMemoryAs response.body .rgba
        let state := TileState.loaded img response.body
        mgr.updateTileState coord state
        -- Save to disk cache on worker thread
        mgr.saveToDiskCache coord response.body
      catch e =>
        -- Decode error - treat as failure
        let msg := s!"Decode error: {e}"
        let rs := RetryState.initialFailure frame msg
        let state := TileState.failed rs
        mgr.updateTileState coord state
        -- Schedule retry check
        scheduleRetryCheck mgr coord rs
    else
      -- HTTP error - treat as failure
      let msg := s!"HTTP {response.status}"
      let rs := RetryState.initialFailure frame msg
      let state := TileState.failed rs
      mgr.updateTileState coord state
      scheduleRetryCheck mgr coord rs
  | .error err =>
    -- Network error
    let msg := match err with
      | .timeoutError s => s!"Timeout: {s}"
      | .connectionError s => s!"Connection: {s}"
      | .sslError s => s!"SSL: {s}"
      | e => s!"{e}"
    let rs ← if wasRetry then do
      -- Get existing retry state and increment
      let cache ← mgr.memoryCacheRef.get
      match cache.get coord with
      | some (.retrying existingRs) => pure (existingRs.recordRetryFailure frame msg)
      | _ => pure (RetryState.initialFailure frame msg)
    else
      pure (RetryState.initialFailure frame msg)
    let state := TileState.failed rs
    mgr.updateTileState coord state
    scheduleRetryCheck mgr coord rs
end

private def fetchFromNetwork (mgr : TileManager) (coord : TileCoord) (wasRetry : Bool) : IO Unit := do
  if (← mgr.isShutdownRef.get) then
    return ()
  if (← mgr.isCanceled coord) then
    return ()
  -- Mark as pending/retrying
  let cache ← mgr.memoryCacheRef.get
  let state := if wasRetry then
    match cache.get coord with
    | some (.failed rs) => TileState.retrying rs
    | _ => TileState.pending
  else
    TileState.pending
  mgr.updateTileState coord state

  -- Start HTTP request and wait for completion on this worker thread
  let url := mgr.config.provider.tileUrl coord
  let cancelPromise ← IO.Promise.new
  let (httpTask, cancelHandle) ← mgr.httpClient.getCancelable url
  mgr.inFlightRef.modify (·.insert coord { cancel := cancelHandle, cancelPromise := cancelPromise })
  let httpWaitTask ← IO.asTask (pure httpTask.get)
  let shutdownWaitTask ← IO.asTask (do
    let _ := (mgr.shutdownPromise.result!).get
    return (Except.error (.ioError "shutdown") : Wisp.WispResult Wisp.Response)
  )
  let cancelWaitTask ← IO.asTask (do
    let _ := (cancelPromise.result!).get
    return (Except.error (.ioError "canceled") : Wisp.WispResult Wisp.Response)
  )
  let result ← IO.waitAny [httpWaitTask, shutdownWaitTask, cancelWaitTask]
  IO.cancel httpWaitTask
  IO.cancel shutdownWaitTask
  IO.cancel cancelWaitTask
  mgr.inFlightRef.modify (·.erase coord)
  if (← mgr.isShutdownRef.get) || (← mgr.isCanceled coord) then
    pure ()
  else
    match result with
    | .ok res =>
        match res with
        | .error (.ioError msg) =>
            if msg == "shutdown" || msg == "canceled" then
              pure ()
            else
              handleFetchComplete mgr coord res wasRetry
        | _ =>
            handleFetchComplete mgr coord res wasRetry
    | .error err =>
        let res : Wisp.WispResult Wisp.Response := .error (.ioError s!"{err}")
        handleFetchComplete mgr coord res wasRetry

private def processLoad (mgr : TileManager) (coord : TileCoord) : IO Unit := do
  -- Try disk cache first (off main thread)
  if let some bytes ← tryDiskCache mgr coord then
    if let some img ← decodeImage coord bytes then
      let state := TileState.loaded img bytes
      mgr.updateTileState coord state
    else
      -- Disk cache corrupted, fall through to network fetch
      fetchFromNetwork mgr coord false
  else
    fetchFromNetwork mgr coord false

private partial def workerLoop (mgr : TileManager) : IO Unit := do
  let rec loop : IO Unit := do
    let job? ← jobQueueRecv mgr.jobQueue
    match job? with
    | none => pure ()
    | some job =>
      try
        if (← mgr.isShutdownRef.get) then
          pure ()
        else if (← isCanceled mgr job.coord) then
          pure ()
        else if (← isPriorityStale mgr job.coord job.generation) then
          pure ()
        else
          match job.kind with
          | .load =>
              processLoad mgr job.coord
          | .fetch =>
              fetchFromNetwork mgr job.coord true
          | .decode bytes =>
              match ← decodeImage job.coord bytes with
              | some img =>
                  let state := TileState.loaded img bytes
                  mgr.updateTileState job.coord state
              | none =>
                  fetchFromNetwork mgr job.coord false
      catch e =>
        IO.eprintln s!"[Tileset] Worker error: {e}"
      loop
  loop

private def startWorkers (mgr : TileManager) : IO (Array (Task (Except IO.Error Unit))) := do
  let count := Nat.max 1 mgr.config.workerCount
  let mut tasks := #[]
  for _ in [:count] do
    let worker ← (workerLoop mgr).asTask Task.Priority.dedicated
    tasks := tasks.push worker
  pure tasks

/-- Create a new TileManager.
    Must be called within SpiderM to get the timeline context. -/
def new (config : TileManagerConfig) : SpiderM TileManager := do
  let httpClient := Wisp.HTTP.Client.new.withTimeout config.httpTimeout
  let memoryCacheRef ← SpiderM.liftIO <| IO.mkRef MemoryCache.empty
  let diskConfig := DiskCacheConfig.fromProvider config.provider config.diskCacheDir config.diskCacheMaxSize
  let diskCacheIndexRef ← SpiderM.liftIO <| IO.mkRef (DiskCache.emptyIndex diskConfig)
  let bindingsRef ← SpiderM.liftIO <| IO.mkRef ({} : HashMap TileCoord TileBinding)
  let inFlightRef ← SpiderM.liftIO <| IO.mkRef ({} : HashMap TileCoord InFlight)
  let canceledRef ← SpiderM.liftIO <| IO.mkRef ({} : HashSet TileCoord)
  let priorityRef ← SpiderM.liftIO <| IO.mkRef ({} : HashMap TileCoord RequestInfo)
  let frameCounterRef ← SpiderM.liftIO <| IO.mkRef 0
  let jobQueue ← SpiderM.liftIO jobQueueNew
  let jobSeqRef ← SpiderM.liftIO <| IO.mkRef 0
  let isShutdownRef ← SpiderM.liftIO <| IO.mkRef false
  let shutdownPromise ← SpiderM.liftIO <| IO.Promise.new
  let mgr := {
    config
    httpClient
    memoryCacheRef
    diskCacheIndexRef
    bindingsRef
    inFlightRef
    canceledRef
    priorityRef
    frameCounterRef
    jobQueue
    jobSeqRef
    workerTasks := #[]
    isShutdownRef
    shutdownPromise
  }
  let workerTasks ← SpiderM.liftIO <| startWorkers mgr
  return { mgr with workerTasks := workerTasks }

/-- Request a tile with explicit priority (higher = sooner).

    The returned Dynamic will emit:
    - `.loading` when the fetch starts
    - `.ready img` when the image is decoded and available
    - `.error msg` if all retries are exhausted

    The tile is automatically fetched from the network or disk cache.
    Updates happen asynchronously - no polling required. -/
def requestTileWithPriority (mgr : TileManager) (coord : TileCoord) (priority : Int)
    (generation : Nat := 0)
    : SpiderM (Dyn TileLoadState) := do
  let wasCanceled ← SpiderM.liftIO do
    let canceled ← mgr.canceledRef.get
    pure (canceled.contains coord)
  SpiderM.liftIO <| mgr.canceledRef.modify (·.erase coord)
  let info ← SpiderM.liftIO <| upsertRequestInfo mgr coord priority generation
  let shouldSchedule ← SpiderM.liftIO do
    match info.lastScheduled with
    | none => pure true
    | some last => pure (last < info.generation)
  -- Check if we already have this tile
  let cache ← SpiderM.liftIO mgr.memoryCacheRef.get
  match cache.get coord with
  | some state =>
    -- Already have state, just return/create binding
    let binding ← mgr.getOrCreateBinding coord state.toLoadState
    if shouldSchedule then
      SpiderM.liftIO <| markScheduled mgr coord info.generation
      match state with
      | .cached bytes _ =>
          SpiderM.liftIO <| mgr.updateTileState coord .pending
          SpiderM.liftIO <| enqueueJob mgr coord (.decode bytes) info.priority info.generation
      | .failed _ | .exhausted _ =>
          SpiderM.liftIO <| mgr.updateTileState coord .pending
          SpiderM.liftIO <| enqueueJob mgr coord .load info.priority info.generation
      | .pending | .retrying _ =>
          let inflight ← SpiderM.liftIO mgr.inFlightRef.get
          if inflight.contains coord then
            pure ()
          else
            SpiderM.liftIO <| mgr.updateTileState coord .pending
            SpiderM.liftIO <| enqueueJob mgr coord .load info.priority info.generation
      | _ =>
          if wasCanceled then
            SpiderM.liftIO <| mgr.updateTileState coord .pending
            SpiderM.liftIO <| enqueueJob mgr coord .load info.priority info.generation
    return binding.dynamic
  | none =>
    -- New tile request (enqueue background load)
    let binding ← mgr.getOrCreateBinding coord .loading
    if shouldSchedule then
      SpiderM.liftIO <| markScheduled mgr coord info.generation
      SpiderM.liftIO <| mgr.updateTileState coord .pending
      SpiderM.liftIO <| enqueueJob mgr coord .load info.priority info.generation
    return binding.dynamic

/-- Request a tile with default priority. -/
def requestTile (mgr : TileManager) (coord : TileCoord)
    : SpiderM (Dyn TileLoadState) :=
  requestTileWithPriority mgr coord 0

/-- Increment the frame counter. Call this once per frame for retry timing.
    Note: This is optional if you don't need frame-accurate retry timing. -/
def tick (mgr : TileManager) : IO Unit := do
  mgr.frameCounterRef.modify (· + 1)

/-- Evict tiles that are far from the visible area.
    Call this periodically to free memory. -/
def evictDistant (mgr : TileManager) (keepSet : HashSet TileCoord) : IO Unit := do
  let frame ← mgr.frameCounterRef.get
  let cache ← mgr.memoryCacheRef.get

  -- Drop priorities for tiles outside the keep set
  let priorities ← mgr.priorityRef.get
  let toDrop := priorities.toList.filter fun (coord, _) => !keepSet.contains coord
  for (coord, _) in toDrop do
    mgr.priorityRef.modify (·.erase coord)

  -- Find loaded tiles to unload (keep raw bytes)
  let toUnload := cache.tilesToUnload keepSet
  for (coord, _img, pngData) in toUnload do
    let state := TileState.cached pngData frame
    mgr.updateTileState coord state

  -- Find stale tiles to remove completely
  let cacheAfterUnload ← mgr.memoryCacheRef.get
  let stale := cacheAfterUnload.staleTiles keepSet
  cancelCoords mgr stale
  mgr.memoryCacheRef.modify (·.removeCoords stale)

  -- Remove bindings for stale tiles
  for coord in stale do
    mgr.bindingsRef.modify (·.erase coord)

  -- Evict oldest cached tiles if over limit
  let cache' ← mgr.memoryCacheRef.get
  let toEvict := cache'.cachedToEvict keepSet mgr.config.unloadConfig.maxLoadedImages
  mgr.memoryCacheRef.modify (·.removeCoords toEvict)

  for coord in toEvict do
    mgr.bindingsRef.modify (·.erase coord)

/-- Shut down background workers and prevent new work. -/
def shutdown (mgr : TileManager) : IO Unit := do
  mgr.isShutdownRef.set true
  try
    mgr.shutdownPromise.resolve ()
  catch _ =>
    pure ()
  let inflight ← mgr.inFlightRef.get
  for (coord, _) in inflight.toList do
    cancelCoord mgr coord
  jobQueueClose mgr.jobQueue
  for task in mgr.workerTasks do
    let _ := task.get

/-- Get current cache statistics: (loaded, cached, other) -/
def stats (mgr : TileManager) : IO (Nat × Nat × Nat) := do
  let cache ← mgr.memoryCacheRef.get
  return cache.stateCounts

/-- Update request priority/generation without enqueuing work. -/
def updateRequestInfo (mgr : TileManager) (coord : TileCoord) (priority : Int) (generation : Nat) : IO Unit := do
  let _ ← upsertRequestInfo mgr coord priority generation
  pure ()

/-- Number of in-flight network requests. -/
def inFlightCount (mgr : TileManager) : IO Nat := do
  let inflight ← mgr.inFlightRef.get
  pure inflight.size

/-- Number of tiles currently marked canceled (until re-requested). -/
def canceledCount (mgr : TileManager) : IO Nat := do
  let canceled ← mgr.canceledRef.get
  pure canceled.size

/-- Check if a specific tile is loaded and ready -/
def isReady (mgr : TileManager) (coord : TileCoord) : IO Bool := do
  let cache ← mgr.memoryCacheRef.get
  match cache.get coord with
  | some (.loaded _ _) => return true
  | _ => return false

/-- Get all currently loaded tiles (for rendering) -/
def loadedTiles (mgr : TileManager) : IO (List (TileCoord × Raster.Image)) := do
  let cache ← mgr.memoryCacheRef.get
  return cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .loaded img _ => some (coord, img)
    | _ => none

end TileManager

end Tileset
