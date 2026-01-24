/-
  Tileset/Cache.lean - Memory and Disk Cache for Tiles

  Provides:
  - MemoryCache: In-memory tile cache with LRU eviction
  - DiskCache: Persistent disk cache using Cellar
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Tileset.Coord
import Tileset.State
import Tileset.Provider
import Cellar

namespace Tileset

open Std (HashMap HashSet)
open Reactive (RetryConfig)

-- ============================================================================
-- Memory Cache
-- ============================================================================

/-- In-memory tile cache using HashMap -/
structure MemoryCache where
  tiles : HashMap TileCoord TileState
  retryConfig : RetryConfig := RetryConfig.default
  unloadConfig : UnloadConfig := defaultUnloadConfig
  deriving Inhabited

namespace MemoryCache

def empty : MemoryCache := { tiles := {}, retryConfig := RetryConfig.default }

def get (cache : MemoryCache) (coord : TileCoord) : Option TileState :=
  cache.tiles[coord]?

def insert (cache : MemoryCache) (coord : TileCoord) (state : TileState) : MemoryCache :=
  { cache with tiles := cache.tiles.insert coord state }

def contains (cache : MemoryCache) (coord : TileCoord) : Bool :=
  cache.tiles.contains coord

def remove (cache : MemoryCache) (coord : TileCoord) : MemoryCache :=
  { cache with tiles := cache.tiles.erase coord }

/-- Count of all tiles in cache -/
def size (cache : MemoryCache) : Nat :=
  cache.tiles.size

/-- Count tiles by state category: (loaded, cached, other) -/
def stateCounts (cache : MemoryCache) : (Nat × Nat × Nat) :=
  cache.tiles.toList.foldl (fun (loaded, cached, other) (_, state) =>
    match state with
    | .loaded _ _ => (loaded + 1, cached, other)
    | .cached _ _ => (loaded, cached + 1, other)
    | _ => (loaded, cached, other + 1)
  ) (0, 0, 0)

/-- Identify loaded tiles outside the keep zone -/
def tilesToUnload (cache : MemoryCache) (keepSet : HashSet TileCoord)
    : List (TileCoord × Raster.Image × ByteArray) :=
  cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .loaded img pngData =>
      if keepSet.contains coord then none else some (coord, img, pngData)
    | _ => none

/-- Identify non-loaded, non-cached tiles outside the keep zone (cheap to remove) -/
def staleTiles (cache : MemoryCache) (keepSet : HashSet TileCoord) : List TileCoord :=
  cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .loaded _ _ => none  -- Handled by tilesToUnload
    | .cached _ _ => none  -- Keep cached data
    | _ => if keepSet.contains coord then none else some coord

/-- Remove tiles from cache by coordinates -/
def removeCoords (cache : MemoryCache) (coords : List TileCoord) : MemoryCache :=
  { cache with tiles := coords.foldl (fun m c => m.erase c) cache.tiles }

/-- Get cached tiles that are in the visible set (need decoding) -/
def cachedTilesToLoad (cache : MemoryCache) (visibleSet : HashSet TileCoord)
    : List (TileCoord × ByteArray) :=
  cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .cached pngData _ => if visibleSet.contains coord then some (coord, pngData) else none
    | _ => none

/-- Count of loaded tiles (with decoded images) -/
def loadedCount (cache : MemoryCache) : Nat :=
  cache.tiles.toList.foldl (fun count (_, state) =>
    match state with
    | .loaded _ _ => count + 1
    | _ => count
  ) 0

/-- Count of cached tiles (raw bytes only) -/
def cachedCount (cache : MemoryCache) : Nat :=
  cache.tiles.toList.foldl (fun count (_, state) =>
    match state with
    | .cached _ _ => count + 1
    | _ => count
  ) 0

/-- Get oldest cached tiles to evict (returns coords sorted by lastAccess, oldest first) -/
def cachedToEvict (cache : MemoryCache) (keepSet : HashSet TileCoord) (maxToKeep : Nat)
    : List TileCoord :=
  -- Get all cached tiles with their access times, excluding those in keepSet
  let cached := cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .cached _ lastAccess =>
      if keepSet.contains coord then none else some (coord, lastAccess)
    | _ => none
  -- Sort by lastAccess (oldest first)
  let sorted := cached.toArray.qsort (fun a b => a.2 < b.2) |>.toList
  -- Calculate how many to evict
  let currentCount := cache.cachedCount
  if currentCount <= maxToKeep then
    []
  else
    let toEvict := currentCount - maxToKeep
    sorted.take toEvict |>.map Prod.fst

/-- Check if a tile is loaded with a decoded image -/
def isLoaded (cache : MemoryCache) (coord : TileCoord) : Bool :=
  match cache.get coord with
  | some (.loaded _ _) => true
  | _ => false

/-- Get all loaded ancestors of a tile (for fallback rendering).
    Walks up through parent tiles, collecting loaded ones. -/
def getLoadedAncestors (cache : MemoryCache) (coord : TileCoord) (maxLevels : Nat := 8)
    : List TileCoord :=
  go coord maxLevels []
where
  go (c : TileCoord) (remaining : Nat) (acc : List TileCoord) : List TileCoord :=
    match remaining with
    | 0 => acc
    | remaining' + 1 =>
      if c.z <= 0 then acc
      else
        let parent := c.parentTile
        -- Keep climbing even if parent is not loaded - grandparent might be!
        let acc' := if cache.isLoaded parent then parent :: acc else acc
        go parent remaining' acc'

end MemoryCache

-- ============================================================================
-- Disk Cache
-- ============================================================================

/-- Configuration for tile disk cache -/
structure DiskCacheConfig where
  /-- Base directory for cached tiles -/
  cacheDir : String := "./tile_cache"
  /-- Tileset name (used in path) -/
  tilesetName : String := "cartodb_dark"
  /-- Maximum total size of cached tiles in bytes -/
  maxSizeBytes : Nat := 2000 * 1024 * 1024  -- 2 GB default
  deriving Repr, Inhabited

namespace DiskCacheConfig

/-- Convert to Cellar.CacheConfig -/
def toCellarConfig (config : DiskCacheConfig) : Cellar.CacheConfig :=
  { cacheDir := config.cacheDir, maxSizeBytes := config.maxSizeBytes }

/-- Create config from provider -/
def fromProvider (provider : TileProvider) (cacheDir : String := "./tile_cache")
    (maxSizeBytes : Nat := 2000 * 1024 * 1024) : DiskCacheConfig := {
  cacheDir := cacheDir
  tilesetName := provider.cacheId
  maxSizeBytes := maxSizeBytes
}

end DiskCacheConfig

/-- Alias for tile cache entry -/
abbrev DiskCacheEntry := Cellar.CacheEntry TileCoord

/-- Alias for tile cache index -/
abbrev DiskCacheIndex := Cellar.CacheIndex TileCoord

/-- Compute file path for a tile: {cacheDir}/{tilesetName}/{z}/{x}/{y}.png -/
def tilePath (config : DiskCacheConfig) (coord : TileCoord) : String :=
  s!"{config.cacheDir}/{config.tilesetName}/{coord.z}/{coord.x}/{coord.y}.png"

namespace DiskCache

/-- Create an empty cache index -/
def emptyIndex (config : DiskCacheConfig) : DiskCacheIndex :=
  Cellar.CacheIndex.empty config.toCellarConfig

/-- Check if a tile exists in the disk cache -/
def exists? (config : DiskCacheConfig) (coord : TileCoord) : IO Bool := do
  Cellar.fileExists (tilePath config coord)

/-- Read a tile from disk cache -/
def read (config : DiskCacheConfig) (coord : TileCoord) : IO (Option ByteArray) := do
  let path := tilePath config coord
  if ← Cellar.fileExists path then
    match ← Cellar.readFile path with
    | .ok bytes => return some bytes
    | .error _ => return none
  else
    return none

/-- Write a tile to disk cache (creates directories as needed) -/
def write (config : DiskCacheConfig) (coord : TileCoord) (data : ByteArray) : IO Unit := do
  let path := tilePath config coord
  -- Create parent directories
  let dir := s!"{config.cacheDir}/{config.tilesetName}/{coord.z}/{coord.x}"
  IO.FS.createDirAll dir
  let _ ← Cellar.writeFile path data
  pure ()

/-- Delete a tile from disk cache -/
def delete (config : DiskCacheConfig) (coord : TileCoord) : IO Unit := do
  let _ ← Cellar.deleteFile (tilePath config coord)
  pure ()

/-- Select entries to evict to stay under size limit -/
def selectEvictions (index : DiskCacheIndex) (newFileSize : Nat) : List DiskCacheEntry :=
  Cellar.selectEvictions index newFileSize

/-- Add an entry to the index -/
def addEntry (index : DiskCacheIndex) (entry : DiskCacheEntry) : DiskCacheIndex :=
  Cellar.addEntry index entry

/-- Remove entries from the index -/
def removeEntries (index : DiskCacheIndex) (evicted : List DiskCacheEntry) : DiskCacheIndex :=
  Cellar.removeEntries index evicted

/-- Touch an entry (update access time) -/
def touchEntry (index : DiskCacheIndex) (key : TileCoord) : IO DiskCacheIndex := do
  let now ← Cellar.nowMs
  return Cellar.touchEntry index key now

end DiskCache

-- Re-export cellar utilities
export Cellar (fileExists readFile writeFile deleteFile nowMs)
export Cellar (wouldExceedLimit)

end Tileset
