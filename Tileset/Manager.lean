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
  deriving Repr, Inhabited

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
  /-- Current frame counter (for LRU tracking and retry timing) -/
  frameCounterRef : IO.Ref Nat
  /-- Timeline context for creating dynamics -/
  timelineCtx : Reactive.TimelineCtx Spider

namespace TileManager

/-- Create a new TileManager.
    Must be called within SpiderM to get the timeline context. -/
def new (config : TileManagerConfig) : SpiderM TileManager := do
  let httpClient := Wisp.HTTP.Client.new.withTimeout config.httpTimeout
  let memoryCacheRef ← SpiderM.liftIO <| IO.mkRef MemoryCache.empty
  let diskConfig := DiskCacheConfig.fromProvider config.provider config.diskCacheDir config.diskCacheMaxSize
  let diskCacheIndexRef ← SpiderM.liftIO <| IO.mkRef (DiskCache.emptyIndex diskConfig)
  let bindingsRef ← SpiderM.liftIO <| IO.mkRef ({} : HashMap TileCoord TileBinding)
  let frameCounterRef ← SpiderM.liftIO <| IO.mkRef 0
  let timelineCtx ← SpiderM.getTimelineCtx
  return {
    config
    httpClient
    memoryCacheRef
    diskCacheIndexRef
    bindingsRef
    frameCounterRef
    timelineCtx
  }

/-- Internal: Create a TileBinding with a Dynamic -/
private def createBinding (mgr : TileManager) (initial : TileLoadState)
    : IO TileBinding := do
  let nodeId ← mgr.timelineCtx.freshNodeId
  let (dynamic, updateFn) ← Dynamic.newWithId (t := Spider) initial nodeId
  return { dynamic, update := updateFn }

/-- Internal: Get or create a binding for a tile -/
private def getOrCreateBinding (mgr : TileManager) (coord : TileCoord)
    (initialState : TileLoadState) : IO TileBinding := do
  let bindings ← mgr.bindingsRef.get
  match bindings[coord]? with
  | some binding => return binding
  | none =>
    let binding ← mgr.createBinding initialState
    mgr.bindingsRef.modify (·.insert coord binding)
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
          startFetchAsync mgr coord true
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
        let img ← Raster.Image.loadFromMemory response.body
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

/-- Internal: Start an async fetch for a tile -/
partial def startFetchAsync (mgr : TileManager) (coord : TileCoord) (wasRetry : Bool)
    : IO Unit := do
  -- Mark as pending/retrying
  let cache ← mgr.memoryCacheRef.get
  let state := if wasRetry then
    match cache.get coord with
    | some (.failed rs) => TileState.retrying rs
    | _ => TileState.pending
  else
    TileState.pending
  mgr.updateTileState coord state

  -- Start HTTP request
  let url := mgr.config.provider.tileUrl coord
  let httpTask ← mgr.httpClient.get url

  -- Spawn background task to handle completion
  let _ ← (do
    let result := httpTask.get  -- Block in background task
    handleFetchComplete mgr coord result wasRetry
  ).asTask
end

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
    let binding ← SpiderM.liftIO <| mgr.getOrCreateBinding coord state.toLoadState
    return binding.dynamic
  | none =>
    -- New tile request
    SpiderM.liftIO do
      -- Try disk cache first
      if let some bytes ← mgr.tryDiskCache coord then
        -- Decode the image
        try
          let img ← Raster.Image.loadFromMemory bytes
          let state := TileState.loaded img bytes
          mgr.updateTileState coord state
          let binding ← mgr.getOrCreateBinding coord state.toLoadState
          return binding.dynamic
        catch e =>
          -- Disk cache corrupted, fall through to network fetch
          IO.eprintln s!"[Tileset] Disk cache decode error for {coord}: {e}"

      -- Create binding with loading state
      let binding ← mgr.getOrCreateBinding coord .loading

      -- Start async fetch (non-blocking)
      mgr.startFetchAsync coord false

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
  let stale := cache.staleTiles keepSet
  let cache' := cache.removeCoords stale
  mgr.memoryCacheRef.set cache'

  -- Remove bindings for stale tiles
  for coord in stale do
    mgr.bindingsRef.modify (·.erase coord)

  -- Evict oldest cached tiles if over limit
  let cache'' ← mgr.memoryCacheRef.get
  let toEvict := cache''.cachedToEvict keepSet mgr.config.unloadConfig.maxLoadedImages
  let cache''' := cache''.removeCoords toEvict
  mgr.memoryCacheRef.set cache'''

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
