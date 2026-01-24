/-
  Tileset/State.lean - GPU-agnostic Tile State

  This module defines tile state types that use Raster.Image instead of
  GPU textures. Consumers can upload images to GPU textures as needed.
-/
import Tileset.Coord
import Reactive.Core.Retry
import Raster

namespace Tileset

open Reactive (RetryConfig RetryState)

/-- Consumer-facing tile load state (what external code sees).
    This is a simple enum without internal retry details. -/
inductive TileLoadState where
  /-- Tile is being fetched from network or decoded -/
  | loading
  /-- Tile is ready with decoded image data -/
  | ready : Raster.Image → TileLoadState
  /-- Tile fetch failed permanently (all retries exhausted) -/
  | error : String → TileLoadState
  deriving Inhabited

instance : Repr TileLoadState where
  reprPrec s _ := match s with
    | .loading => "TileLoadState.loading"
    | .ready img => s!"TileLoadState.ready (Image {img.width}×{img.height})"
    | .error msg => s!"TileLoadState.error \"{msg}\""

instance : BEq TileLoadState where
  beq
    | .loading, .loading => true
    | .ready a, .ready b => a == b
    | .error a, .error b => a == b
    | _, _ => false

/-- Internal tile state for the cache manager.
    Tracks more details like retry state and raw bytes for disk cache. -/
inductive TileState where
  /-- Initial fetch in flight -/
  | pending
  /-- Loaded with decoded image + raw PNG bytes (for disk cache/reload) -/
  | loaded : Raster.Image → ByteArray → TileState
  /-- Unloaded from memory, raw PNG in disk cache (Nat = last access frame for LRU) -/
  | cached : ByteArray → Nat → TileState
  /-- Failed, can be retried -/
  | failed : RetryState → TileState
  /-- Retry fetch in flight -/
  | retrying : RetryState → TileState
  /-- All retries exhausted, permanent failure -/
  | exhausted : RetryState → TileState
  deriving Inhabited

instance : Repr TileState where
  reprPrec s _ := match s with
    | .pending => "TileState.pending"
    | .loaded img bytes => s!"TileState.loaded (Image {img.width}×{img.height}) ({bytes.size} bytes)"
    | .cached bytes frame => s!"TileState.cached ({bytes.size} bytes) (frame {frame})"
    | .failed rs => s!"TileState.failed {repr rs}"
    | .retrying rs => s!"TileState.retrying {repr rs}"
    | .exhausted rs => s!"TileState.exhausted {repr rs}"

namespace TileState

/-- Convert internal state to consumer-facing state -/
def toLoadState (state : TileState) : TileLoadState :=
  match state with
  | .pending => .loading
  | .loaded img _ => .ready img
  | .cached _ _ => .loading  -- Will be decoded soon
  | .failed _ => .loading    -- Will retry soon
  | .retrying _ => .loading
  | .exhausted rs => .error (rs.lastError.getD "Unknown error")

/-- Check if the tile is currently being loaded (fetch or retry in flight) -/
def isLoading (state : TileState) : Bool :=
  match state with
  | .pending => true
  | .retrying _ => true
  | _ => false

/-- Check if the tile has a decoded image ready -/
def isReady (state : TileState) : Bool :=
  match state with
  | .loaded _ _ => true
  | _ => false

/-- Check if the tile has failed permanently -/
def isExhausted (state : TileState) : Bool :=
  match state with
  | .exhausted _ => true
  | _ => false

/-- Get the image if loaded -/
def getImage? (state : TileState) : Option Raster.Image :=
  match state with
  | .loaded img _ => some img
  | _ => none

/-- Get the raw PNG bytes if available -/
def getRawBytes? (state : TileState) : Option ByteArray :=
  match state with
  | .loaded _ bytes => some bytes
  | .cached bytes _ => some bytes
  | _ => none

end TileState

/-- Result from a background fetch task -/
structure FetchResult where
  coord : TileCoord
  result : Except String (Raster.Image × ByteArray)  -- Decoded image + original PNG bytes
  wasRetry : Bool := false  -- Track if this was a retry attempt

/-- Configuration for tile unloading behavior -/
structure UnloadConfig where
  /-- Extra tiles beyond visible area to keep images loaded -/
  bufferTiles : Int := 3
  /-- Max decoded images to keep in memory -/
  maxLoadedImages : Nat := 200
  /-- Max raw PNG bytes to keep in memory (before disk cache) -/
  maxCachedBytes : Nat := 100 * 1024 * 1024  -- 100 MB
  deriving Repr, Inhabited

def defaultUnloadConfig : UnloadConfig := {}

end Tileset
