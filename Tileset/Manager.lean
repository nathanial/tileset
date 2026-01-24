/-
  Tileset/Manager.lean - Reactive Tile Manager (WorkerPool-based)

  The TileManager provides a reactive interface for tile loading:
  - Each tile request returns a Dynamic that updates when state changes
  - Fetching is done via wisp HTTP client with background tasks
  - Two-tier caching: memory (Raster.Image) + disk (Cellar)
  - Fully reactive: no polling required, Dynamics update automatically

  This implementation uses WorkerPool.fromCommandsChainable for:
  - Job chaining: load → fetch → decode
  - Priority queue ordering
  - Generation-based soft cancellation
  - Cancel handle support for HTTP requests
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Tileset.Coord
import Tileset.Provider
import Tileset.State
import Tileset.Cache
import Reactive.Core.Retry
import Reactive.Host.Spider.Async
import Raster
import Wisp
import Reactive

namespace Tileset

open Std (HashMap HashSet)
open Reactive (Dynamic Event RetryConfig RetryState)
open Reactive.Host

/-- Configuration for the TileManager -/
structure TileManagerConfig where
  /-- Tile provider (defines URL template, tile size, etc.) -/
  provider : TileProvider := TileProvider.default
  /-- Disk cache directory -/
  diskCacheDir : String := "./tile_cache"
  /-- Maximum disk cache size in bytes -/
  diskCacheMaxSize : Nat := 2000 * 1024 * 1024  -- 2 GB
  /-- Retry configuration -/
  retryConfig : RetryConfig := RetryConfig.default
  /-- Unload configuration -/
  unloadConfig : UnloadConfig := defaultUnloadConfig
  /-- HTTP client timeout in milliseconds -/
  httpTimeout : UInt64 := 30000
  /-- Number of background workers for tile IO and decoding -/
  workerCount : Nat := 4
  deriving Repr, Inhabited

/-- Job types for the tile worker pool.
    Each variant carries the TileCoord so the processor can access it. -/
inductive TileJob where
  /-- Initial request: check disk cache, may spawn fetch or decode -/
  | load (coord : TileCoord)
  /-- Network fetch (spawns decode on success) -/
  | fetch (coord : TileCoord)
  /-- Decode bytes to image -/
  | decode (coord : TileCoord) (bytes : ByteArray)
  deriving Inhabited

namespace TileJob

/-- Extract the coordinate from a TileJob -/
def coord : TileJob → TileCoord
  | .load c => c
  | .fetch c => c
  | .decode c _ => c

end TileJob

instance : Repr TileJob where
  reprPrec j _ := match j with
    | .load c => s!"TileJob.load {repr c}"
    | .fetch c => s!"TileJob.fetch {repr c}"
    | .decode c bytes => s!"TileJob.decode {repr c} ({bytes.size} bytes)"

/-- Result of processing a tile job -/
inductive TileResult where
  /-- Final result: decoded image ready for display -/
  | decoded (img : Raster.Image) (bytes : ByteArray)
  /-- Disk cache hit: bytes loaded, needs decode -/
  | diskHit (bytes : ByteArray)
  /-- Cache miss: no disk cache, needs fetch -/
  | cacheMiss
  /-- Fetch success: bytes downloaded, needs decode -/
  | fetched (bytes : ByteArray)
  /-- Error during processing -/
  | error (msg : String)
  deriving Inhabited

instance : Repr TileResult where
  reprPrec r _ := match r with
    | .decoded img bytes => s!"TileResult.decoded (Image {img.width}×{img.height}) ({bytes.size} bytes)"
    | .diskHit bytes => s!"TileResult.diskHit ({bytes.size} bytes)"
    | .cacheMiss => "TileResult.cacheMiss"
    | .fetched bytes => s!"TileResult.fetched ({bytes.size} bytes)"
    | .error msg => s!"TileResult.error \"{msg}\""

/-- Internal state for a tile's reactive binding -/
private structure TileBinding where
  /-- The Dynamic that consumers subscribe to -/
  dynamic : Dyn TileLoadState
  /-- Function to update the dynamic's value -/
  update : TileLoadState → IO Unit

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
  /-- Current frame counter (for LRU tracking) -/
  frameCounterRef : IO.Ref Nat
  /-- Function to fire pool commands -/
  fireCommand : PoolCommand TileCoord TileJob → IO Unit
  /-- Pool shutdown handle -/
  poolHandle : WorkerPool.PoolHandle
  /-- Pool running count dynamic -/
  runningCountDyn : Dyn Nat
  /-- Retry scheduler (handles exponential backoff automatically) -/
  retryScheduler : RetryScheduler TileCoord
  /-- Fire application-level errors to retry scheduler -/
  fireAppError : TileCoord × String → IO Unit
  /-- Canceled tiles (skip queued work until re-requested) -/
  canceledRef : IO.Ref (HashSet TileCoord)
  /-- Shutdown flag -/
  isShutdownRef : IO.Ref Bool

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

/-- Internal: Try to load from disk cache -/
private def tryDiskCache (config : TileManagerConfig) (coord : TileCoord)
    : IO (Option ByteArray) := do
  let diskConfig := DiskCacheConfig.fromProvider config.provider
    config.diskCacheDir config.diskCacheMaxSize
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

/-- Internal: Decode PNG bytes to image -/
private def decodeImage (bytes : ByteArray) : IO (Option Raster.Image) := do
  try
    let img ← Raster.Image.loadFromMemoryAs bytes .rgba
    pure (some img)
  catch _ =>
    pure none

/-- Internal: Fetch tile from network -/
private def fetchTileBytes (client : Wisp.HTTP.Client) (provider : TileProvider)
    (coord : TileCoord) : IO (Except String ByteArray) := do
  let url := provider.tileUrl coord
  let task ← client.get url
  let result := task.get
  match result with
  | .ok response =>
    if response.status >= 200 && response.status < 300 then
      pure (Except.ok response.body)
    else
      pure (Except.error s!"HTTP {response.status}")
  | .error err =>
    let msg := match err with
      | .timeoutError s => s!"Timeout: {s}"
      | .connectionError s => s!"Connection: {s}"
      | .sslError s => s!"SSL: {s}"
      | e => s!"{e}"
    pure (Except.error msg)

/-- Internal: Check if an error is a network error (retryable) -/
private def isNetworkError (msg : String) : Bool :=
  msg.startsWith "Timeout:" ||
  msg.startsWith "Connection:" ||
  msg.startsWith "SSL:" ||
  msg.startsWith "HTTP 5"  -- Server errors are retryable

/-- Handle completed tile job results.
    Network errors are forwarded to the retry scheduler via fireAppError.
    Non-retryable errors are marked as exhausted immediately. -/
private def handleJobResult (mgr : TileManager) (coord : TileCoord) (_job : TileJob)
    (result : TileResult) : IO Unit := do
  match result with
  | .decoded img bytes =>
    -- Final state: loaded successfully
    updateTileState mgr coord (.loaded img bytes)

  | .diskHit _ | .cacheMiss | .fetched _ =>
    -- Intermediate states - follow-ups will continue processing
    pure ()

  | .error msg =>
    if isNetworkError msg then
      -- Forward to retry scheduler (it will handle backoff and resubmit)
      mgr.fireAppError (coord, msg)
    else
      -- Non-retryable error - mark as exhausted immediately
      let now ← IO.monoMsNow
      let rs := RetryState.initialFailure now msg
      updateTileState mgr coord (.exhausted rs)

/-- Create the ChainableProcessor for tile jobs -/
private def mkTileProcessor (config : TileManagerConfig) (httpClient : Wisp.HTTP.Client)
    (diskCacheIndexRef : IO.Ref DiskCacheIndex)
    : ChainableProcessor TileCoord TileJob TileResult := {
  process := fun job => do
    let coord := job.coord
    match job with
    | .load _ =>
      -- Check disk cache first
      match ← tryDiskCache config coord with
      | some bytes =>
        -- Disk hit - spawn decode job
        pure {
          result := .diskHit bytes
          followUps := #[(coord, .decode coord bytes, 0)]
        }
      | none =>
        -- Cache miss - spawn fetch job
        pure {
          result := .cacheMiss
          followUps := #[(coord, .fetch coord, 0)]
        }

    | .fetch _ =>
      -- Fetch from network
      match ← fetchTileBytes httpClient config.provider coord with
      | .ok bytes =>
        -- Save to disk cache (fire and forget)
        let diskConfig := DiskCacheConfig.fromProvider config.provider
          config.diskCacheDir config.diskCacheMaxSize
        let saveToDisk : IO Unit := do
          let index ← diskCacheIndexRef.get
          let evictions := DiskCache.selectEvictions index bytes.size
          evictions.forM fun entry => do
            try DiskCache.delete diskConfig entry.key catch _ => pure ()
          let index' := DiskCache.removeEntries index evictions
          try
            DiskCache.write diskConfig coord bytes
            let now ← Cellar.nowMs
            let entry : DiskCacheEntry := {
              key := coord
              filePath := tilePath diskConfig coord
              sizeBytes := bytes.size
              lastAccessTime := now
            }
            let index'' := DiskCache.addEntry index' entry
            diskCacheIndexRef.set index''
          catch _ => pure ()
        let _ ← saveToDisk.asTask
        -- Spawn decode job
        pure {
          result := .fetched bytes
          followUps := #[(coord, .decode coord bytes, 0)]
        }
      | .error msg =>
        pure { result := .error msg }

    | .decode _ bytes =>
      -- Decode the image
      match ← decodeImage bytes with
      | some img =>
        pure { result := .decoded img bytes }
      | none =>
        pure { result := .error "Failed to decode image" }

  createCancelHandle := some fun job => do
    match job with
    | .fetch _ =>
      -- For true HTTP cancellation, we would need to use Wisp's getCancelable
      -- and track the handle. For now, soft cancellation via generation counters
      -- handles most cases effectively.
      pure none
    | _ => pure none
}

/-- Create a new TileManager.
    Must be called within SpiderM to get the timeline context. -/
def new (config : TileManagerConfig) : SpiderM TileManager := do
  -- Create HTTP client
  let httpClient := Wisp.HTTP.Client.new.withTimeout config.httpTimeout

  -- Create refs for state
  let memoryCacheRef ← SpiderM.liftIO <| IO.mkRef MemoryCache.empty
  let diskConfig := DiskCacheConfig.fromProvider config.provider config.diskCacheDir config.diskCacheMaxSize
  let diskCacheIndexRef ← SpiderM.liftIO <| IO.mkRef (DiskCache.emptyIndex diskConfig)
  let bindingsRef ← SpiderM.liftIO <| IO.mkRef ({} : HashMap TileCoord TileBinding)
  let frameCounterRef ← SpiderM.liftIO <| IO.mkRef 0
  let canceledRef ← SpiderM.liftIO <| IO.mkRef ({} : HashSet TileCoord)
  let isShutdownRef ← SpiderM.liftIO <| IO.mkRef false

  -- Create command stream for the pool
  let (commandEvt, fireCommand) ← Reactive.newTriggerEvent (t := Spider)
    (a := PoolCommand TileCoord TileJob)

  -- Create chainable pool with the tile processor
  let processor := mkTileProcessor config httpClient diskCacheIndexRef
  let poolConfig : WorkerPoolConfig := { workerCount := config.workerCount }
  let (poolOutput, poolHandle) ← WorkerPool.fromCommandsChainable poolConfig processor commandEvt

  -- Create application-level error event for retry scheduler
  -- (handles TileResult.error from completed jobs, not just thrown exceptions)
  let (appErrorEvt, fireAppError) ← Reactive.newTriggerEvent (t := Spider)
    (a := TileCoord × String)

  -- Merge pool errors with application errors for unified retry handling
  let allErrors ← Event.mergeM poolOutput.errored appErrorEvt

  -- Create retry scheduler using the Reactive library combinator
  let retryScheduler ← withRetryScheduling
    config.retryConfig
    (fun (_, msg) => isNetworkError msg)  -- Only retry network errors
    (fun coord => TileJob.fetch coord)     -- Recreate fetch job for retry
    { poolOutput with errored := allErrors }  -- Use merged error stream
    fireCommand

  -- Build the manager (note: we need mgr ref for subscriptions, use partial init)
  let mgrRef ← SpiderM.liftIO <| IO.mkRef (none : Option TileManager)

  let mgr : TileManager := {
    config
    httpClient
    memoryCacheRef
    diskCacheIndexRef
    bindingsRef
    frameCounterRef
    fireCommand
    poolHandle
    runningCountDyn := poolOutput.runningCount
    retryScheduler
    fireAppError
    canceledRef
    isShutdownRef
  }

  SpiderM.liftIO <| mgrRef.set (some mgr)

  -- Subscribe to completed results (forwards errors to retry scheduler via fireAppError)
  let _ ← poolOutput.completed.subscribe fun (coord, job, result) =>
    handleJobResult mgr coord job result

  -- Subscribe to retry scheduler events for UI state updates
  let _ ← retryScheduler.retryScheduled.subscribe fun (coord, delayMs) => do
    let now ← IO.monoMsNow
    let rs : RetryState := { lastAttemptTime := now, lastError := some s!"Retrying in {delayMs}ms" }
    updateTileState mgr coord (.retrying rs)

  let _ ← retryScheduler.exhausted.subscribe fun (coord, errMsg) => do
    let now ← IO.monoMsNow
    let rs := RetryState.initialFailure now errMsg
    updateTileState mgr coord (.exhausted rs)

  -- Subscribe to cancelled jobs to track in canceledRef
  let _ ← poolOutput.cancelled.subscribe fun coord =>
    mgr.canceledRef.modify (·.insert coord)

  return mgr

/-- Request a tile with explicit priority (higher = sooner).

    The returned Dynamic will emit:
    - `.loading` when the fetch starts
    - `.ready img` when the image is decoded and available
    - `.error msg` if all retries are exhausted

    The tile is automatically fetched from the network or disk cache.
    Updates happen asynchronously - no polling required. -/
def requestTileWithPriority (mgr : TileManager) (coord : TileCoord) (priority : Int)
    : SpiderM (Dyn TileLoadState) := do
  -- Check if we already have this tile in memory cache
  let cache ← SpiderM.liftIO mgr.memoryCacheRef.get
  match cache.get coord with
  | some state =>
    -- Already have state, get or create binding
    let binding ← mgr.getOrCreateBinding coord state.toLoadState
    match state with
    | .loaded _ _ =>
      -- Already loaded, just return
      pure binding.dynamic
    | .pending | .retrying _ =>
      -- In progress, update priority
      SpiderM.liftIO <| mgr.fireCommand (.updatePriority coord priority)
      pure binding.dynamic
    | .cached bytes _ =>
      -- Cached bytes, need to decode - clear canceled flag
      SpiderM.liftIO <| mgr.canceledRef.modify (·.erase coord)
      SpiderM.liftIO <| mgr.updateTileState coord .pending
      SpiderM.liftIO <| mgr.fireCommand (.submit coord (.decode coord bytes) priority)
      pure binding.dynamic
    | .failed _ | .exhausted _ =>
      -- Failed, try again - clear canceled flag
      SpiderM.liftIO <| mgr.canceledRef.modify (·.erase coord)
      SpiderM.liftIO <| mgr.updateTileState coord .pending
      SpiderM.liftIO <| mgr.fireCommand (.submit coord (.load coord) priority)
      pure binding.dynamic

  | none =>
    -- New tile request - clear canceled flag if set
    SpiderM.liftIO <| mgr.canceledRef.modify (·.erase coord)
    let binding ← mgr.getOrCreateBinding coord .loading
    SpiderM.liftIO <| mgr.updateTileState coord .pending
    SpiderM.liftIO <| mgr.fireCommand (.submit coord (.load coord) priority)
    return binding.dynamic

/-- Request a tile with default priority. -/
def requestTile (mgr : TileManager) (coord : TileCoord)
    : SpiderM (Dyn TileLoadState) :=
  requestTileWithPriority mgr coord 0

/-- Increment the frame counter. Call this once per frame for retry timing. -/
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

  -- Find stale tiles to remove completely and cancel
  let cacheAfterUnload ← mgr.memoryCacheRef.get
  let stale := cacheAfterUnload.staleTiles keepSet
  for coord in stale do
    -- Cancel any in-progress work via pool command and retry scheduler
    mgr.fireCommand (.cancel coord)
    mgr.retryScheduler.cancelRetry coord
    mgr.bindingsRef.modify (·.erase coord)
  mgr.memoryCacheRef.modify (·.removeCoords stale)

  -- Evict oldest cached tiles if over limit
  let cache' ← mgr.memoryCacheRef.get
  let toEvict := cache'.cachedToEvict keepSet mgr.config.unloadConfig.maxLoadedImages
  mgr.memoryCacheRef.modify (·.removeCoords toEvict)

  for coord in toEvict do
    mgr.bindingsRef.modify (·.erase coord)

/-- Shut down background workers and prevent new work. -/
def shutdown (mgr : TileManager) : IO Unit := do
  mgr.isShutdownRef.set true
  mgr.poolHandle.shutdown

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

/-- Get the number of currently in-flight requests (running jobs in the pool) -/
def inFlightCount (mgr : TileManager) : IO Nat := do
  mgr.runningCountDyn.sample

/-- Get the number of tiles that have been canceled since last check -/
def canceledCount (mgr : TileManager) : IO Nat := do
  let canceled ← mgr.canceledRef.get
  return canceled.size

/-- Clear the canceled tiles set (call after checking canceledCount) -/
def clearCanceled (mgr : TileManager) : IO Unit := do
  mgr.canceledRef.set {}

end TileManager

end Tileset
