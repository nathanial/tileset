# CLAUDE.md - Tileset

GPU-agnostic tile loading and caching library for map tiles in Lean 4.

## Build

```bash
lake build && lake test
```

No special build script needed - curl linking is handled in the lakefile.

## Architecture

### Core Types

| Type | Location | Purpose |
|------|----------|---------|
| `TileCoord` | `Tileset/Coord.lean` | Tile coordinates (x, y, z) + Web Mercator projection |
| `TileProvider` | `Tileset/Provider.lean` | URL templates for tile servers (CartoDB, OSM, Stamen) |
| `TileLoadState` | `Tileset/State.lean` | Public state: loading, ready, error |
| `TileState` | `Tileset/State.lean` | Internal state with cached bytes, retry info |
| `TileManager` | `Tileset/Manager.lean` | Main API - reactive tile loading with caching |
| `MapViewport` | `Tileset/Viewport.lean` | Visible tile calculation for screen regions |

### Caching Strategy

Two-tier caching:
1. **Memory**: `MemoryCache` holds decoded `Raster.Image` or raw PNG bytes
2. **Disk**: `Cellar`-based disk cache in `diskCacheDir`

Eviction flow:
- `loaded` (decoded image) → `cached` (raw bytes only) → removed from memory
- Disk cache uses LRU eviction when size limit exceeded

### Worker Pool

`TileManager` uses `WorkerPool.fromCommandsChainable` for async loading:

```
TileJob.load → check disk cache
  ├─ disk hit → TileJob.decode
  └─ cache miss → TileJob.fetch → TileJob.decode
```

Jobs support priority ordering and generation-based soft cancellation.

### Reactive Interface

Each `requestTile` returns a `Dyn TileLoadState` that updates automatically:
- `.loading` - fetch in progress
- `.ready img` - image decoded and available
- `.error msg` - retries exhausted

No polling required - subscribe to changes via `Dynamic.updated`.

## Key Functions

```lean
-- Create manager in SpiderM context
TileManager.new : TileManagerConfig → SpiderM TileManager

-- Request tile (returns auto-updating Dynamic)
TileManager.requestTile : TileManager → TileCoord → SpiderM (Dyn TileLoadState)

-- Memory management
TileManager.evictDistant : TileManager → HashSet TileCoord → IO Unit

-- Coordinate conversion
latLonToTile : LatLon → Nat → TileCoord
tileToLatLon : TileCoord → LatLon

-- Viewport calculations
MapViewport.visibleTiles : MapViewport → List TileCoord
MapViewport.visibleTileSetWithFallbackDepths : MapViewport → Int → Nat → Nat → HashSet TileCoord
```

## Dependencies

- `cellar` - disk caching primitives
- `wisp` - HTTP client for tile fetching
- `raster` - PNG decoding
- `reactive` - FRP primitives (Dynamic, Event, SpiderM)
- `crucible` - test framework

## Testing

Tests cover:
- Coordinate math and projection round-trips
- Provider URL generation
- Viewport visible tile calculation
- Memory cache operations
- Retry logic
- Network integration (requires internet)

Run specific test suite:
```bash
lake exe tileset_tests "TileCoord"
lake exe tileset_tests "Viewport"
```
