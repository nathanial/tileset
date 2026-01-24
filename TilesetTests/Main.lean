/-
  Tileset Tests
-/
import Crucible
import Tileset

-- ============================================================================
-- TileCoord Tests
-- ============================================================================

namespace TilesetTests.Coord

open Crucible
open Tileset

testSuite "TileCoord"

test "parentTile gives correct quadrant" := do
  let child : TileCoord := { x := 5, y := 7, z := 3 }
  let parent := child.parentTile
  parent ≡ { x := 2, y := 3, z := 2 : TileCoord }

test "childTiles returns 4 children" := do
  let parent : TileCoord := { x := 1, y := 1, z := 1 }
  let children := parent.childTiles
  children.size ≡ 4
  -- All children should be at z+1
  for child in children do
    child.z ≡ 2

test "childTiles and parentTile are inverse" := do
  let parent : TileCoord := { x := 3, y := 5, z := 4 }
  let children := parent.childTiles
  -- Each child's parent should be the original parent
  for child in children do
    child.parentTile ≡ parent

test "ancestorAt steps to target zoom" := do
  let coord : TileCoord := { x := 5, y := 7, z := 3 }
  TileCoord.ancestorAt coord 3 ≡ coord
  TileCoord.ancestorAt coord 2 ≡ { x := 2, y := 3, z := 2 : TileCoord }
  TileCoord.ancestorAt coord 1 ≡ { x := 1, y := 1, z := 1 : TileCoord }

end TilesetTests.Coord

-- ============================================================================
-- Projection Tests
-- ============================================================================

namespace TilesetTests.Projection

open Crucible
open Tileset

testSuite "Projection"

test "latLonToTile at zoom 0 gives (0,0)" := do
  let pos : LatLon := { lat := 0.0, lon := 0.0 }
  let tile := latLonToTile pos 0
  tile.z ≡ 0
  tile.x ≡ 0
  tile.y ≡ 0

test "tileToLatLon for (0,0,0) gives northwest corner" := do
  let tile : TileCoord := { x := 0, y := 0, z := 0 }
  let pos := tileToLatLon tile
  -- Northwest corner of world tile
  ensure (pos.lat > 80.0) "lat should be near max latitude"
  ensure (pos.lon == -180.0) "lon should be -180"

test "round-trip preserves tile at higher zoom" := do
  -- At higher zoom, round-trip should preserve tile
  let originalTile : TileCoord := { x := 12345, y := 6789, z := 15 }
  let latLon := tileToLatLon originalTile
  let backToTile := latLonToTile latLon originalTile.z
  backToTile ≡ originalTile

test "tilesAtZoom is 2^zoom" := do
  (tilesAtZoom 0) ≡ 1
  (tilesAtZoom 1) ≡ 2
  (tilesAtZoom 2) ≡ 4
  (tilesAtZoom 10) ≡ 1024

end TilesetTests.Projection

-- ============================================================================
-- Provider Tests
-- ============================================================================

namespace TilesetTests.Provider

open Crucible
open Tileset

testSuite "TileProvider"

test "tileUrl generates correct URL" := do
  let tile : TileCoord := { x := 123, y := 456, z := 10 }
  let url := TileProvider.cartoDarkRetina.tileUrl tile
  shouldContainSubstr url "/10/123/456@2x.png"
  shouldContainSubstr url "basemaps.cartocdn.com"

test "tileUrl rotates subdomains" := do
  -- Same coordinates should give same subdomain (for caching)
  let tile1 : TileCoord := { x := 0, y := 0, z := 0 }
  let tile2 : TileCoord := { x := 1, y := 0, z := 0 }
  let url1 := TileProvider.openStreetMap.tileUrl tile1
  let url2 := TileProvider.openStreetMap.tileUrl tile2
  -- Different tiles may have different subdomains
  shouldContainSubstr url1 "tile.openstreetmap.org"
  shouldContainSubstr url2 "tile.openstreetmap.org"

test "cacheId is filesystem-safe" := do
  let id := TileProvider.cartoDarkRetina.cacheId
  ensure (not (id.any (· == ' '))) "cacheId should not contain spaces"
  ensure (not (id.any (· == '@'))) "cacheId should not contain @"
  id ≡ "cartodb-dark-2x"

test "clampZoom respects provider limits" := do
  let provider := TileProvider.stamenWatercolor  -- maxZoom = 16
  (provider.clampZoom 20) ≡ 16
  (provider.clampZoom 10) ≡ 10
  (provider.clampZoom (-1)) ≡ 0

end TilesetTests.Provider

-- ============================================================================
-- Retry Logic Tests
-- ============================================================================

namespace TilesetTests.Retry

open Crucible
open Tileset
open Reactive (RetryConfig RetryState)

testSuite "RetryLogic"

test "initial failure has zero retry count" := do
  let state := RetryState.initialFailure 100 "test error"
  state.retryCount ≡ 0
  state.lastAttemptTime ≡ 100

test "recordRetryFailure increments count" := do
  let state := RetryState.initialFailure 100 "test error"
  let state' := state.recordRetryFailure 200 "retry error"
  state'.retryCount ≡ 1
  state'.lastAttemptTime ≡ 200

test "isExhausted after max retries" := do
  let config : RetryConfig := { maxRetries := 3, baseDelayMs := 1000 }
  let state := RetryState.initialFailure 0 "error"
    |>.recordRetryFailure 100 "error"
    |>.recordRetryFailure 200 "error"
    |>.recordRetryFailure 300 "error"
  ensure (state.isExhausted config) "state should be exhausted after max retries"

test "backoffDelayMs is exponential" := do
  let config : RetryConfig := { maxRetries := 3, baseDelayMs := 1000 }
  let s0 := RetryState.initialFailure 0 "error"
  let s1 := s0.recordRetryFailure 100 "error"
  let s2 := s1.recordRetryFailure 200 "error"
  (s0.backoffDelayMs config) ≡ 1000   -- 1000 * 2^0
  (s1.backoffDelayMs config) ≡ 2000   -- 1000 * 2^1
  (s2.backoffDelayMs config) ≡ 4000   -- 1000 * 2^2

test "canRetry while retries remain" := do
  let config : RetryConfig := { maxRetries := 3, baseDelayMs := 1000 }
  let state := RetryState.initialFailure 100 "error"
  ensure (state.canRetry config) "should be able to retry initially"
  let state' := state.recordRetryFailure 200 "error"
    |>.recordRetryFailure 300 "error"
    |>.recordRetryFailure 400 "error"
  ensure (not (state'.canRetry config)) "should not retry after max retries"

end TilesetTests.Retry

-- ============================================================================
-- Viewport Tests
-- ============================================================================

namespace TilesetTests.Viewport

open Crucible
open Tileset
open Std (HashSet)

private def wrappedTileX (centerX : Float) (tilesPerAxis : Float) (tileX : Float) : Float :=
  let candidates := #[tileX - tilesPerAxis, tileX, tileX + tilesPerAxis]
  candidates.foldl (fun best c =>
    if Float.abs (c - centerX) < Float.abs (best - centerX) then c else best
  ) tileX

private def tileTopLeft (vp : MapViewport) (tile : TileCoord) : (Float × Float) :=
  let (centerX, centerY) := vp.centerTilePos
  let tilesPerAxis := intToFloat (tilesAtZoom vp.zoom)
  let tileX := wrappedTileX centerX tilesPerAxis (intToFloat tile.x)
  let offsetX := (tileX - centerX) * (intToFloat vp.tileSize) +
    (intToFloat vp.screenWidth) / 2.0
  let offsetY := (intToFloat tile.y - centerY) * (intToFloat vp.tileSize) +
    (intToFloat vp.screenHeight) / 2.0
  (offsetX, offsetY)

private def tileIntersectsViewport (vp : MapViewport) (buffer : Int) (tile : TileCoord) : Bool :=
  let (x, y) := tileTopLeft vp tile
  let tileSize := intToFloat vp.tileSize
  let bufferPx := intToFloat buffer * tileSize
  let viewLeft := -bufferPx
  let viewTop := -bufferPx
  let viewRight := (intToFloat vp.screenWidth) + bufferPx
  let viewBottom := (intToFloat vp.screenHeight) + bufferPx
  let tileRight := x + tileSize
  let tileBottom := y + tileSize
  tileRight > viewLeft && x < viewRight && tileBottom > viewTop && y < viewBottom

private def expectedVisibleTiles (vp : MapViewport) (buffer : Int) : HashSet TileCoord :=
  let count := (tilesAtZoom vp.zoom).toNat
  Id.run do
    let mut set : HashSet TileCoord := {}
    for y in [0:count] do
      for x in [0:count] do
        let tile : TileCoord := { x := natToInt x, y := natToInt y, z := vp.zoom }
        if tileIntersectsViewport vp buffer tile then
          set := set.insert tile
    return set

testSuite "Viewport"

test "visibleTiles returns non-empty list" := do
  let vp : MapViewport := {
    centerLat := 37.7749
    centerLon := -122.4194
    zoom := 12
    screenWidth := 1920
    screenHeight := 1080
    tileSize := 512
  }
  let tiles := vp.visibleTiles
  ensure (tiles.length > 0) "tiles should be non-empty"
  -- All tiles should be at the viewport's zoom level
  for tile in tiles do
    tile.z ≡ 12

test "visibleTilesWithBuffer returns more tiles" := do
  let vp : MapViewport := {
    centerLat := 0.0
    centerLon := 0.0
    zoom := 5
    screenWidth := 800
    screenHeight := 600
    tileSize := 256
  }
  let base := vp.visibleTiles
  let buffered := vp.visibleTilesWithBuffer 2
  ensure (buffered.length > base.length) "buffered tiles should be more than base"

test "visibleTileSet creates HashSet" := do
  let vp : MapViewport := {
    centerLat := 0.0
    centerLon := 0.0
    zoom := 3
    screenWidth := 512
    screenHeight := 512
    tileSize := 256
  }
  let set := vp.visibleTileSet 1
  -- Should contain the center tile
  let centerTile := latLonToTile { lat := 0.0, lon := 0.0 } 3
  ensure (set.contains centerTile) "set should contain center tile"

test "visibleTileSetWithFallbackDepths includes parent and child tiles" := do
  let vp : MapViewport := {
    centerLat := 0.0
    centerLon := 0.0
    zoom := 2
    screenWidth := 256
    screenHeight := 256
    tileSize := 256
  }
  let centerTile := latLonToTile { lat := 0.0, lon := 0.0 } 2
  let set := vp.visibleTileSetWithFallbackDepths 0 1 1
  ensure (set.contains centerTile) "set should contain center tile"
  ensure (set.contains centerTile.parentTile) "set should contain parent tile"
  let child := centerTile.childTiles[0]!
  ensure (set.contains child) "set should contain child tile"

test "centerTilePos returns fractional position" := do
  let vp : MapViewport := {
    centerLat := 0.0
    centerLon := 0.0
    zoom := 2
    screenWidth := 512
    screenHeight := 512
  }
  let (x, y) := vp.centerTilePos
  -- At lat=0, lon=0, zoom=2: should be center of 4x4 grid
  ensure (x > 1.9 && x < 2.1) "x should be approximately 2.0"
  ensure (y > 1.9 && y < 2.1) "y should be approximately 2.0"

test "visibleTilesWithBuffer excludes tiles outside viewport (buffer 0)" := do
  let vp : MapViewport := {
    centerLat := 0.0
    centerLon := 0.0
    zoom := 2
    screenWidth := 512
    screenHeight := 512
    tileSize := 256
  }
  let tiles := vp.visibleTilesWithBuffer 0
  for tile in tiles do
    ensure (tileIntersectsViewport vp 0 tile)
      s!"tile {tile} should intersect viewport"

test "visibleTilesWithBuffer matches geometric coverage (buffer 1)" := do
  let vp : MapViewport := {
    centerLat := 37.7749
    centerLon := -122.4194
    zoom := 3
    screenWidth := 800
    screenHeight := 600
    tileSize := 256
  }
  let expected := expectedVisibleTiles vp 1
  let actual : HashSet TileCoord :=
    (vp.visibleTilesWithBuffer 1).foldl (fun s t => s.insert t) {}
  for tile in expected.toList do
    ensure (actual.contains tile)
      s!"missing tile {tile} in visible set"
  for tile in actual.toList do
    ensure (expected.contains tile)
      s!"extra tile {tile} outside viewport coverage"

end TilesetTests.Viewport

-- ============================================================================
-- State Tests
-- ============================================================================

namespace TilesetTests.State

open Crucible
open Tileset

testSuite "TileState"

test "pending converts to loading" := do
  let state := TileState.pending
  state.toLoadState ≡ TileLoadState.loading

test "loaded converts to ready" := do
  let img : Raster.Image := { width := 512, height := 512, format := .rgba, data := .empty }
  let state := TileState.loaded img .empty
  match state.toLoadState with
  | .ready _ => pure ()
  | _ => throw <| IO.userError "Expected ready state"

test "exhausted converts to error" := do
  let rs := Reactive.RetryState.initialFailure 0 "test error"
  let state := TileState.exhausted rs
  match state.toLoadState with
  | .error msg => msg ≡ "test error"
  | _ => throw <| IO.userError "Expected error state"

test "getImage? returns image when loaded" := do
  let img : Raster.Image := { width := 256, height := 256, format := .rgb, data := .empty }
  let state := TileState.loaded img .empty
  match state.getImage? with
  | some i => i.width ≡ 256
  | none => throw <| IO.userError "Expected some image"

test "getImage? returns none for pending" := do
  let state := TileState.pending
  match state.getImage? with
  | none => pure ()
  | some _ => throw <| IO.userError "Expected none"

end TilesetTests.State

-- ============================================================================
-- Cache Tests
-- ============================================================================

namespace TilesetTests.Cache

open Crucible
open Tileset

testSuite "MemoryCache"

test "empty cache has size 0" := do
  let cache := MemoryCache.empty
  cache.size ≡ 0

test "insert and get" := do
  let coord : TileCoord := { x := 1, y := 2, z := 3 }
  let state := TileState.pending
  let cache := MemoryCache.empty.insert coord state
  match cache.get coord with
  | some s => ensure s.isLoading "state should be loading"
  | none => throw <| IO.userError "Expected some state"

test "contains returns true after insert" := do
  let coord : TileCoord := { x := 5, y := 5, z := 5 }
  let cache := MemoryCache.empty.insert coord TileState.pending
  ensure (cache.contains coord) "cache should contain inserted coord"
  ensure (not (cache.contains { x := 0, y := 0, z := 0 })) "cache should not contain other coord"

test "stateCounts tracks categories" := do
  let img : Raster.Image := { width := 1, height := 1, format := .rgba, data := .empty }
  let cache := MemoryCache.empty
    |>.insert { x := 1, y := 1, z := 1 } (TileState.loaded img .empty)
    |>.insert { x := 2, y := 2, z := 2 } (TileState.cached .empty 0)
    |>.insert { x := 3, y := 3, z := 3 } TileState.pending
  let (loaded, cached, other) := cache.stateCounts
  loaded ≡ 1
  cached ≡ 1
  other ≡ 1

test "cachedTilesToLoad filters by visible set" := do
  let c1 : TileCoord := { x := 1, y := 1, z := 2 }
  let c2 : TileCoord := { x := 2, y := 2, z := 2 }
  let cache := MemoryCache.empty
    |>.insert c1 (TileState.cached .empty 0)
    |>.insert c2 (TileState.cached .empty 0)
  let visible : Std.HashSet TileCoord := ({ } : Std.HashSet TileCoord).insert c1
  let results := cache.cachedTilesToLoad visible
  ensure (results.any (fun (coord, _) => coord == c1)) "visible cached tile should be returned"
  ensure (results.all (fun (coord, _) => coord != c2)) "non-visible cached tile should be filtered"

test "tilesToUnload and staleTiles respect keep set" := do
  let loadedCoord : TileCoord := { x := 0, y := 0, z := 1 }
  let cachedCoord : TileCoord := { x := 1, y := 0, z := 1 }
  let pendingCoord : TileCoord := { x := 2, y := 0, z := 1 }
  let img := Raster.Image.create 1 1 .rgba [0, 0, 0, 255]
  let cache := MemoryCache.empty
    |>.insert loadedCoord (TileState.loaded img .empty)
    |>.insert cachedCoord (TileState.cached .empty 0)
    |>.insert pendingCoord TileState.pending
  let keepSet : Std.HashSet TileCoord := ({ } : Std.HashSet TileCoord).insert cachedCoord
  let toUnload := cache.tilesToUnload keepSet
  ensure (toUnload.any (fun (coord, _, _) => coord == loadedCoord)) "loaded tile outside keep set should unload"
  let stale := cache.staleTiles keepSet
  ensure (stale.contains pendingCoord) "pending tile outside keep set should be stale"
  ensure (!stale.contains cachedCoord) "cached tile should not be stale"

test "removeCoords removes tiles" := do
  let cache := MemoryCache.empty
    |>.insert { x := 1, y := 1, z := 1 } TileState.pending
    |>.insert { x := 2, y := 2, z := 2 } TileState.pending
    |>.insert { x := 3, y := 3, z := 3 } TileState.pending
  let cache' := cache.removeCoords [{ x := 1, y := 1, z := 1 }, { x := 3, y := 3, z := 3 }]
  cache'.size ≡ 1
  ensure (cache'.contains { x := 2, y := 2, z := 2 }) "cache should contain remaining coord"

end TilesetTests.Cache

-- ============================================================================
-- TileManager Tests
-- ============================================================================

namespace TilesetTests.Manager

open Crucible
open Tileset
open Reactive.Host

testSuite "TileManager"
def waitForReady (dyn : Dyn TileLoadState)
    (timeoutMs : Nat := 2000) (stepMs : Nat := 20) : IO Bool := do
  let stepMs := if stepMs == 0 then 1 else stepMs
  let steps := timeoutMs / stepMs
  let rec loop : Nat → IO Bool
    | 0 => do
        let state ← dyn.sample
        match state with
        | .ready _ => pure true
        | .error msg => throw <| IO.userError s!"Unexpected tile error: {msg}"
        | _ => pure false
    | n + 1 => do
        let state ← dyn.sample
        match state with
        | .ready _ => pure true
        | .error msg => throw <| IO.userError s!"Unexpected tile error: {msg}"
        | _ =>
            IO.sleep stepMs.toUInt32
            loop n
  loop steps

def shutdownManager (mgr : TileManager) : IO Unit := do
  TileManager.shutdown mgr
  pure ()

test "evictDistant preserves bytes in cached state" := do
  let env ← SpiderEnv.new
  let coord : TileCoord := { x := 2, y := 4, z := 6 }
  let img := Raster.Image.create 2 2 .rgba [255, 0, 0, 255]
  let bytes ← Raster.Image.encode img .png
  let mgr ← (TileManager.new { workerCount := 1 }).run env
  try
    mgr.memoryCacheRef.modify (·.insert coord (TileState.loaded img bytes))
    let keepSet : Std.HashSet TileCoord := {}
    TileManager.evictDistant mgr keepSet
    let cache ← mgr.memoryCacheRef.get
    match cache.get coord with
    | some (.cached cachedBytes _) => cachedBytes.size ≡ bytes.size
    | _ => throw <| IO.userError "Expected cached state after eviction"
  finally
    shutdownManager mgr
    env.currentScope.dispose

test "cached tile rehydrates after eviction" := do
  let env ← SpiderEnv.new
  let coord : TileCoord := { x := 1, y := 2, z := 3 }
  let img := Raster.Image.create 2 2 .rgba [0, 255, 0, 255]
  let bytes ← Raster.Image.encode img .png
  let mgr ← (TileManager.new { workerCount := 1 }).run env
  try
    mgr.memoryCacheRef.modify (·.insert coord (TileState.loaded img bytes))
    let keepSet : Std.HashSet TileCoord := {}
    TileManager.evictDistant mgr keepSet
    let dyn ← (TileManager.requestTile mgr coord).run env
    let ready ← waitForReady dyn
    ensure ready "cached tile should decode to ready"
    let finalState ← dyn.sample
    match finalState with
    | .ready readyImg => readyImg.width ≡ 2
    | _ => throw <| IO.userError "Expected ready tile after decode"
  finally
    shutdownManager mgr
    env.currentScope.dispose

test "disk cache hit avoids network fetch" := do
  let env ← SpiderEnv.new
  let coord : TileCoord := { x := 1, y := 1, z := 1 }
  let cacheDir ← do
    let stamp ← IO.monoMsNow
    pure s!"/tmp/tileset_test_cache_{stamp}"
  let config : TileManagerConfig := {
    provider := TileProvider.openStreetMap
    diskCacheDir := cacheDir
    httpTimeout := 1000
    retryConfig := { maxRetries := 0, baseDelayMs := 100 }
    workerCount := 1
  }
  let diskConfig := DiskCacheConfig.fromProvider config.provider cacheDir config.diskCacheMaxSize
  let img := Raster.Image.create 2 2 .rgba [0, 0, 255, 255]
  let bytes ← Raster.Image.encode img .png
  DiskCache.write diskConfig coord bytes
  let mgr ← (TileManager.new config).run env
  try
    let dyn ← (TileManager.requestTile mgr coord).run env
    let ready ← waitForReady dyn (timeoutMs := 2000) (stepMs := 20)
    ensure ready "cached tile should load from disk without network"
    let inflight ← TileManager.inFlightCount mgr
    inflight ≡ 0
  finally
    shutdownManager mgr
    env.currentScope.dispose

end TilesetTests.Manager

-- ============================================================================
-- TileManager Network Tests
-- ============================================================================

namespace TilesetTests.ManagerNetwork

open Crucible
open Tileset
open Reactive.Host

testSuite "TileManager Network"

beforeAll := do
  Wisp.FFI.globalInit

afterAll := do
  Wisp.FFI.globalCleanup
  Wisp.HTTP.Client.shutdown

private def freshCacheDir : IO String := do
  let stamp ← IO.monoMsNow
  pure s!"/tmp/tileset_test_cache_{stamp}"

private def waitForDiskCache (config : DiskCacheConfig) (coord : TileCoord)
    (timeoutMs : Nat := 2000) (stepMs : Nat := 50) : IO Bool := do
  let stepMs := if stepMs == 0 then 1 else stepMs
  let steps := timeoutMs / stepMs
  let rec loop : Nat → IO Bool
    | 0 => DiskCache.exists? config coord
    | n + 1 => do
        if (← DiskCache.exists? config coord) then
          pure true
        else
          IO.sleep stepMs.toUInt32
          loop n
  loop steps

test "network tile loads" (timeout := 20000) := do
  let env ← SpiderEnv.new
  let coord : TileCoord := { x := 0, y := 0, z := 0 }
  let cacheDir ← freshCacheDir
  let config : TileManagerConfig := {
    provider := TileProvider.openStreetMap
    diskCacheDir := cacheDir
    httpTimeout := 10000
    retryConfig := { maxRetries := 0, baseDelayMs := 100 }
    workerCount := 2
  }
  let mgr ← (TileManager.new config).run env
  try
    let dyn ← (TileManager.requestTile mgr coord).run env
    let ready ← TilesetTests.Manager.waitForReady dyn (timeoutMs := 15000) (stepMs := 100)
    ensure ready "network tile should load"
    let finalState ← dyn.sample
    match finalState with
    | .ready img =>
        let expected := config.provider.tileSize.toNat
        img.width ≡ expected
        img.height ≡ expected
    | _ => throw <| IO.userError "Expected ready tile after network fetch"
  finally
    TilesetTests.Manager.shutdownManager mgr
    env.currentScope.dispose

test "network tile writes disk cache" (timeout := 20000) := do
  let env ← SpiderEnv.new
  let coord : TileCoord := { x := 0, y := 0, z := 0 }
  let cacheDir ← freshCacheDir
  let config : TileManagerConfig := {
    provider := TileProvider.openStreetMap
    diskCacheDir := cacheDir
    httpTimeout := 10000
    retryConfig := { maxRetries := 0, baseDelayMs := 100 }
    workerCount := 2
  }
  let diskConfig := DiskCacheConfig.fromProvider config.provider cacheDir config.diskCacheMaxSize
  let mgr ← (TileManager.new config).run env
  try
    let dyn ← (TileManager.requestTile mgr coord).run env
    let ready ← TilesetTests.Manager.waitForReady dyn (timeoutMs := 15000) (stepMs := 100)
    ensure ready "network tile should load"
    let cached ← waitForDiskCache diskConfig coord (timeoutMs := 5000) (stepMs := 100)
    ensure cached "disk cache entry should exist after download"
  finally
    TilesetTests.Manager.shutdownManager mgr
    env.currentScope.dispose

test "evict cancels pending tile" (timeout := 10000) := do
  let env ← SpiderEnv.new
  let coord : TileCoord := { x := 0, y := 0, z := 0 }
  let slowProvider : TileProvider := {
    name := "blackhole"
    urlTemplate := "http://10.255.255.1/{z}/{x}/{y}.png"
    subdomains := #["a"]
    tileSize := 256
    maxZoom := 19
    minZoom := 0
    attribution := ""
  }
  let config : TileManagerConfig := {
    provider := slowProvider
    httpTimeout := 60000
    retryConfig := { maxRetries := 0, baseDelayMs := 100 }
    workerCount := 1
  }
  let mgr ← (TileManager.new config).run env
  try
    let _dyn ← (TileManager.requestTile mgr coord).run env
    -- Evict immediately to cancel any in-flight request.
    let keepSet : Std.HashSet TileCoord := {}
    TileManager.evictDistant mgr keepSet
    for _ in [:60] do
      let inflight ← TileManager.inFlightCount mgr
      let canceled ← TileManager.canceledCount mgr
      if inflight == 0 && canceled > 0 then
        break
      IO.sleep 50
    let inflight ← TileManager.inFlightCount mgr
    let canceled ← TileManager.canceledCount mgr
    inflight ≡ 0
    ensure (canceled > 0) "expected canceled tile request"
  finally
    TilesetTests.Manager.shutdownManager mgr
    env.currentScope.dispose

test "re-request clears canceled flag" (timeout := 10000) := do
  let env ← SpiderEnv.new
  let coord : TileCoord := { x := 1, y := 1, z := 1 }
  let slowProvider : TileProvider := {
    name := "blackhole"
    urlTemplate := "http://10.255.255.1/{z}/{x}/{y}.png"
    subdomains := #["a"]
    tileSize := 256
    maxZoom := 19
    minZoom := 0
    attribution := ""
  }
  let config : TileManagerConfig := {
    provider := slowProvider
    httpTimeout := 60000
    retryConfig := { maxRetries := 0, baseDelayMs := 100 }
    workerCount := 1
  }
  let mgr ← (TileManager.new config).run env
  try
    let _ ← (TileManager.requestTile mgr coord).run env
    let keepSet : Std.HashSet TileCoord := {}
    TileManager.evictDistant mgr keepSet
    for _ in [:60] do
      let inflight ← TileManager.inFlightCount mgr
      let canceled ← TileManager.canceledCount mgr
      if inflight == 0 && canceled > 0 then
        break
      IO.sleep 50
    let canceledBefore ← TileManager.canceledCount mgr
    ensure (canceledBefore > 0) "expected canceled flag before re-request"
    let _ ← (TileManager.requestTile mgr coord).run env
    let canceledAfter ← TileManager.canceledCount mgr
    ensure (canceledAfter < canceledBefore) "expected canceled flag cleared on re-request"
  finally
    TilesetTests.Manager.shutdownManager mgr
    env.currentScope.dispose

end TilesetTests.ManagerNetwork

-- ============================================================================
-- MapBounds Tests
-- ============================================================================

namespace TilesetTests.Bounds

open Crucible
open Tileset

testSuite "MapBounds"

test "world bounds contain equator" := do
  ensure (MapBounds.world.contains 0.0 0.0) "world bounds should contain equator"

test "usa bounds contain San Francisco" := do
  ensure (MapBounds.usa.contains 37.7749 (-122.4194)) "USA bounds should contain San Francisco"

test "usa bounds do not contain London" := do
  ensure (not (MapBounds.usa.contains 51.5074 (-0.1278))) "USA bounds should not contain London"

test "clampZoom respects bounds" := do
  let bounds := MapBounds.usa
  (bounds.clampZoom 1) ≡ 3  -- USA has minZoom 3
  (bounds.clampZoom 25) ≡ 19  -- USA has maxZoom 19
  (bounds.clampZoom 10) ≡ 10

end TilesetTests.Bounds

-- ============================================================================
-- Main Test Runner
-- ============================================================================

open Crucible in
def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
