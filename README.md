# Tileset

A GPU-agnostic tile loading and caching library for Lean 4. Handles map tile fetching, caching, and reactive state management without coupling to any specific graphics backend.

## Features

- **Web Mercator Projection** - Convert between lat/lon and tile coordinates
- **Multiple Tile Providers** - CartoDB, OpenStreetMap, Stamen presets included
- **Two-tier Caching** - In-memory LRU cache + disk cache via Cellar
- **Reactive Interface** - Each tile returns a `Dynamic` that updates automatically
- **Automatic Retries** - Exponential backoff for failed requests
- **Viewport Calculations** - Compute visible tiles for a given map view

## Installation

Add to your `lakefile.lean`:

```lean
require tileset from git "https://github.com/nathanial/tileset" @ "v0.0.1"
```

## Usage

```lean
import Tileset

open Tileset

-- Create a tile manager
def main : IO Unit := do
  -- Run in SpiderM for reactive support
  Reactive.Spider.run do
    let config : TileManagerConfig := {
      provider := TileProvider.cartoDbLight
      maxMemoryTiles := 256
      diskCachePath := ".tile-cache"
    }
    let mgr ← TileManager.new config

    -- Request a tile - returns a Dynamic that updates when loaded
    let tileDyn ← mgr.requestTile ⟨0, 0, 1⟩

    -- Read current state
    let state ← tileDyn.read
    match state with
    | .loading => IO.println "Loading..."
    | .ready img => IO.println s!"Ready: {img.width}x{img.height}"
    | .error msg => IO.println s!"Error: {msg}"
```

## Modules

| Module | Description |
|--------|-------------|
| `Tileset.Coord` | `TileCoord`, `LatLon`, Web Mercator projection |
| `Tileset.Provider` | `TileProvider` with URL generation, presets |
| `Tileset.State` | `TileLoadState` (loading/ready/error) |
| `Tileset.Cache` | `MemoryCache`, `DiskCache` wrappers |
| `Tileset.Manager` | `TileManager` with reactive `Dynamic` interface |
| `Tileset.Viewport` | `MapViewport`, visible tile calculation |
| `Tileset.Retry` | `RetryConfig`, exponential backoff logic |

## Dependencies

- [cellar](https://github.com/nathanial/cellar) - Disk caching
- [wisp](https://github.com/nathanial/wisp) - HTTP client
- [raster](https://github.com/nathanial/raster) - Image decoding
- [reactive](https://github.com/nathanial/reactive) - FRP primitives

## API

### TileManager

```lean
-- Create a new manager
def TileManager.new (config : TileManagerConfig) : SpiderM TileManager

-- Request a tile (returns Dynamic that auto-updates)
def TileManager.requestTile (mgr : TileManager) (coord : TileCoord)
    : SpiderM (Dyn TileLoadState)

-- Optional: advance frame counter for retry timing
def TileManager.tick (mgr : TileManager) : IO Unit

-- Evict tiles not in keep set
def TileManager.evictDistant (mgr : TileManager) (keepSet : HashSet TileCoord) : IO Unit

-- Query helpers
def TileManager.isReady (mgr : TileManager) (coord : TileCoord) : IO Bool
def TileManager.loadedTiles (mgr : TileManager) : IO (List (TileCoord × Raster.Image))
```

### TileLoadState

```lean
inductive TileLoadState where
  | loading              -- Fetch in progress
  | ready : Raster.Image → TileLoadState  -- Image available
  | error : String → TileLoadState        -- Permanent failure
```

### Coordinate Conversion

```lean
-- Lat/lon to tile coordinate
def TileCoord.fromLatLon (lat lon : Float) (zoom : Nat) : TileCoord

-- Tile coordinate to lat/lon (northwest corner)
def TileCoord.toLatLon (coord : TileCoord) : LatLon

-- Pixel within tile for precise positioning
def TileCoord.pixelOffset (lat lon : Float) (zoom : Nat) (tileSize : Nat) : (Nat × Nat)
```

## License

MIT
