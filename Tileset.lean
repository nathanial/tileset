/-
  Tileset - GPU-agnostic tile caching library for map tiles

  This library provides:
  - TileCoord: Tile coordinates and Web Mercator projection
  - TileProvider: Configurable tile source providers (CartoDB, OSM, etc.)
  - TileManager: Reactive tile loading with memory/disk caching
  - Viewport: Visible tile calculation utilities

  Example usage:
  ```lean
  import Tileset

  open Tileset
  open Reactive.Host

  def example : SpiderM Unit := do
    let config : TileManagerConfig := {
      provider := TileProvider.cartoDarkRetina
      diskCacheDir := "./tile_cache"
    }
    let mgr ← TileManager.new config

    -- Request a tile - returns a Dynamic that updates when state changes
    let tileDyn ← mgr.requestTile { x := 0, y := 0, z := 0 }

    -- Subscribe to state changes
    let _ ← tileDyn.updated.subscribe fun state =>
      match state with
      | .ready img => IO.println s!"Tile loaded: {img.width}x{img.height}"
      | .loading => IO.println "Loading..."
      | .error msg => IO.println s!"Error: {msg}"

    pure ()
  ```
-/

import Tileset.Coord
import Tileset.Provider
import Tileset.Retry
import Tileset.State
import Tileset.Cache
import Tileset.Manager
import Tileset.Viewport
