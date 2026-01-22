/-
  Tileset/Coord.lean - Tile Coordinates and Web Mercator Projection

  Provides:
  - TileCoord: x, y coordinates at zoom level z
  - LatLon: Geographic coordinates (latitude/longitude)
  - Web Mercator projection functions (EPSG:3857)
-/

namespace Tileset

/-- Pi constant -/
def pi : Float := 3.14159265358979323846

/-- Maximum latitude for Web Mercator projection (degrees) -/
def maxMercatorLatitude : Float := 85.0

/-- Minimum zoom level -/
def minZoomLevel : Int := 0

/-- Maximum zoom level -/
def maxZoomLevel : Int := 19

/-- Default tile size for @2x retina tiles -/
def defaultTileSize : Int := 512

/-- Convert Int to Float -/
@[inline] def intToFloat (n : Int) : Float := Float.ofInt n

/-- Convert Nat to Int -/
@[inline] def natToInt (n : Nat) : Int := n

/-- Max of two integers -/
@[inline] def intMax (a b : Int) : Int := if a > b then a else b

/-- Min of two integers -/
@[inline] def intMin (a b : Int) : Int := if a < b then a else b

/-- Max of two floats -/
@[inline] def floatMax (a b : Float) : Float := if a > b then a else b

/-- Min of two floats -/
@[inline] def floatMin (a b : Float) : Float := if a < b then a else b

/-- Clamp float to range [min, max] -/
@[inline] def floatClamp (value min max : Float) : Float :=
  floatMin max (floatMax min value)

/-- Clamp integer to range [min, max] -/
@[inline] def intClamp (value min max : Int) : Int :=
  intMin max (intMax min value)

/-- Clamp latitude to valid Mercator range -/
@[inline] def clampLatitude (lat : Float) : Float :=
  floatClamp lat (-maxMercatorLatitude) maxMercatorLatitude

/-- Wrap longitude to [-180, 180] range -/
@[inline] def wrapLongitude (lon : Float) : Float :=
  if lon > 180.0 then lon - 360.0
  else if lon < -180.0 then lon + 360.0
  else lon

/-- Clamp zoom level to valid range [0, 19] -/
@[inline] def clampZoom (z : Int) : Int :=
  intClamp z minZoomLevel maxZoomLevel

/-- Tile coordinates (x, y at zoom level z) -/
structure TileCoord where
  x : Int
  y : Int
  z : Int  -- zoom level (0-19 for OSM)
  deriving Repr, BEq, Hashable, Inhabited

/-- Geographic coordinates -/
structure LatLon where
  lat : Float  -- -90 to 90
  lon : Float  -- -180 to 180
  deriving Repr, BEq, Inhabited

/-- Convert latitude/longitude to tile coordinates at given zoom level.
    Uses Web Mercator projection (EPSG:3857). -/
def latLonToTile (pos : LatLon) (zoom : Int) : TileCoord :=
  let n := Float.pow 2.0 (intToFloat zoom)
  let x := natToInt ((pos.lon + 180.0) / 360.0 * n).floor.toUInt64.toNat
  let latRad := pos.lat * pi / 180.0
  let y := natToInt ((1.0 - Float.log (Float.tan latRad + 1.0 / Float.cos latRad) / pi) / 2.0 * n).floor.toUInt64.toNat
  { x := x, y := y, z := zoom }

/-- Convert tile coordinates to latitude/longitude (northwest corner of tile). -/
def tileToLatLon (tile : TileCoord) : LatLon :=
  let n := Float.pow 2.0 (intToFloat tile.z)
  let lon := (intToFloat tile.x) / n * 360.0 - 180.0
  let latRad := Float.atan (Float.sinh (pi * (1.0 - 2.0 * (intToFloat tile.y) / n)))
  let lat := latRad * 180.0 / pi
  { lat := lat, lon := lon }

/-- Number of tiles at a given zoom level (per axis). -/
def tilesAtZoom (zoom : Int) : Int :=
  natToInt (Float.pow 2.0 (intToFloat zoom)).toUInt64.toNat

namespace TileCoord

/-- Get parent tile at zoom z-1 (integer division gives correct quadrant) -/
def parentTile (coord : TileCoord) : TileCoord :=
  { x := coord.x / 2, y := coord.y / 2, z := coord.z - 1 }

/-- Get the 4 child tiles at zoom z+1 that this tile covers -/
def childTiles (coord : TileCoord) : Array TileCoord :=
  let x2 := coord.x * 2
  let y2 := coord.y * 2
  #[ { x := x2,     y := y2,     z := coord.z + 1 }
   , { x := x2 + 1, y := y2,     z := coord.z + 1 }
   , { x := x2,     y := y2 + 1, z := coord.z + 1 }
   , { x := x2 + 1, y := y2 + 1, z := coord.z + 1 } ]

/-- Get the ancestor tile at a specific zoom level (clamps to self if target is deeper). -/
def ancestorAt (coord : TileCoord) (targetZoom : Int) : TileCoord :=
  if coord.z <= targetZoom then
    coord
  else
    let dz : Nat := (coord.z - targetZoom).toNat
    let divisor : Int := natToInt (Nat.pow 2 dz)
    { x := coord.x / divisor, y := coord.y / divisor, z := targetZoom }

/-- Convert to string for debugging -/
def toString (coord : TileCoord) : String :=
  s!"({coord.z}/{coord.x}/{coord.y})"

instance : ToString TileCoord := ⟨TileCoord.toString⟩

end TileCoord

end Tileset
