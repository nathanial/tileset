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

private inductive TileJob where
  | load (coord : TileCoord)
  | fetch (coord : TileCoord)
  | decode (coord : TileCoord) (bytes : ByteArray)

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
  /-- Current frame counter (for LRU tracking and retry timing) -/
  frameCounterRef : IO.Ref Nat
  /-- Background job queue for tile IO/decoding -/
  workerQueue : Std.CloseableChannel.Sync TileJob
  /-- Background worker tasks -/
  workerTasks : Array (Task (Except IO.Error Unit))

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

private def enqueueJob (mgr : TileManager) (job : TileJob) : IO Unit := do
  let _ ← Std.CloseableChannel.Sync.send mgr.workerQueue job
  pure ()

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

mutual
/-- Internal: Schedule a retry check after the backoff delay -/
partial def scheduleRetryCheck (mgr : TileManager) (coord : TileCoord) (rs : RetryState)
    : IO Unit := do
  if rs.isExhausted mgr.config.retryConfig then
    -- No more retries - mark as exhausted
    let state := TileState.exhausted rs
    mgr.updateTileState coord state
  else
    -- Schedule retry after backoff delay
    let delayMs := rs.backoffDelay mgr.config.retryConfig * 16  -- ~16ms per frame at 60fps
    let retryAction : IO Unit := do
      IO.sleep delayMs.toUInt32
      -- Check if tile is still in failed state (not evicted or replaced)
      let cache ← mgr.memoryCacheRef.get
      match cache.get coord with
      | some (.failed currentRs) =>
        if currentRs == rs then  -- Still the same failure
          enqueueJob mgr (TileJob.fetch coord)
        else
          pure ()
      | _ => pure ()  -- Tile state changed, skip retry
    let _ ← retryAction.asTask
    pure ()

/-- Internal: Handle fetch completion (called from background task) -/
partial def handleFetchComplete (mgr : TileManager) (coord : TileCoord)
    (result : Wisp.WispResult Wisp.Response) (wasRetry : Bool) : IO Unit := do
  let frame ← mgr.frameCounterRef.get
  match result with
  | .ok response =>
    if response.status >= 200 && response.status < 300 then
      -- Success - decode the image
      try
        let img ← Raster.Image.loadFromMemoryAs response.body .rgba
        let state := TileState.loaded img response.body
        mgr.updateTileState coord state
        -- Save to disk cache in background (fire and forget)
        let _ ← (mgr.saveToDiskCache coord response.body).asTask
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
  let httpTask ← mgr.httpClient.get url
  let result := httpTask.get
  handleFetchComplete mgr coord result wasRetry

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
    let job? ← mgr.workerQueue.recv
    match job? with
    | none => pure ()
    | some job =>
      try
        match job with
        | .load coord => processLoad mgr coord
        | .fetch coord => fetchFromNetwork mgr coord true
        | .decode coord bytes =>
          match ← decodeImage coord bytes with
          | some img =>
              let state := TileState.loaded img bytes
              mgr.updateTileState coord state
          | none =>
              fetchFromNetwork mgr coord false
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
  let frameCounterRef ← SpiderM.liftIO <| IO.mkRef 0
  let workerQueue ← SpiderM.liftIO Std.CloseableChannel.Sync.new
  let mgr := {
    config
    httpClient
    memoryCacheRef
    diskCacheIndexRef
    bindingsRef
    frameCounterRef
    workerQueue
    workerTasks := #[]
  }
  let workerTasks ← SpiderM.liftIO <| startWorkers mgr
  return { mgr with workerTasks := workerTasks }

/-- Request a tile. Returns a Dynamic that updates when the tile's state changes.

    The returned Dynamic will emit:
    - `.loading` when the fetch starts
    - `.ready img` when the image is decoded and available
    - `.error msg` if all retries are exhausted

    The tile is automatically fetched from the network or disk cache.
    Updates happen asynchronously - no polling required. -/
def requestTile (mgr : TileManager) (coord : TileCoord)
    : SpiderM (Dyn TileLoadState) := do
  -- Check if we already have this tile
  let cache ← SpiderM.liftIO mgr.memoryCacheRef.get
  match cache.get coord with
  | some state =>
    -- Already have state, just return/create binding
    let binding ← mgr.getOrCreateBinding coord state.toLoadState
    match state with
    | .cached bytes _ =>
        SpiderM.liftIO <| mgr.updateTileState coord .pending
        SpiderM.liftIO <| enqueueJob mgr (TileJob.decode coord bytes)
    | _ => pure ()
    return binding.dynamic
  | none =>
    -- New tile request (enqueue background load)
    let binding ← mgr.getOrCreateBinding coord .loading
    SpiderM.liftIO <| mgr.updateTileState coord .pending
    SpiderM.liftIO <| enqueueJob mgr (TileJob.load coord)
    return binding.dynamic

/-- Increment the frame counter. Call this once per frame for retry timing.
    Note: This is optional if you don't need frame-accurate retry timing. -/
def tick (mgr : TileManager) : IO Unit := do
  mgr.frameCounterRef.modify (· + 1)

/-- Evict tiles that are far from the visible area.
    Call this periodically to free memory. -/
def evictDistant (mgr : TileManager) (keepSet : HashSet TileCoord) : IO Unit := do
  let frame ← mgr.frameCounterRef.get
  let cache ← mgr.memoryCacheRef.get

  -- Find loaded tiles to unload (keep raw bytes)
  let toUnload := cache.tilesToUnload keepSet
  for (coord, _img, pngData) in toUnload do
    let state := TileState.cached pngData frame
    mgr.updateTileState coord state

  -- Find stale tiles to remove completely
  let cacheAfterUnload ← mgr.memoryCacheRef.get
  let stale := cacheAfterUnload.staleTiles keepSet
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

/-- Get current cache statistics: (loaded, cached, other) -/
def stats (mgr : TileManager) : IO (Nat × Nat × Nat) := do
  let cache ← mgr.memoryCacheRef.get
  return cache.stateCounts

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
