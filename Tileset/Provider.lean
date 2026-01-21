/-
  Tileset/Provider.lean - Tile Provider Configuration

  Allows configuring different tile sources (CartoDB, OSM, Stamen, custom).
  Each provider defines URL templates, tile sizes, and attribution info.
-/
import Tileset.Coord

namespace Tileset

/-- Configuration for a tile provider -/
structure TileProvider where
  /-- Display name for the provider -/
  name : String
  /-- URL template with placeholders: {s} for subdomain, {z}/{x}/{y} for tile coords -/
  urlTemplate : String
  /-- Available subdomains for load balancing -/
  subdomains : Array String := #["a", "b", "c", "d"]
  /-- Tile size in pixels (256 for standard, 512 for @2x retina) -/
  tileSize : Int := 256
  /-- Attribution text (for display) -/
  attribution : String := ""
  /-- Maximum zoom level supported -/
  maxZoom : Int := 19
  /-- Minimum zoom level supported -/
  minZoom : Int := 0
  deriving Repr, Inhabited

namespace TileProvider

/-- Generate tile URL for a given coordinate using this provider -/
def tileUrl (provider : TileProvider) (tile : TileCoord) : String :=
  -- Select subdomain based on tile coordinates for consistent caching
  let subdomainIdx := (tile.x.toNat + tile.y.toNat) % provider.subdomains.size
  let subdomain := provider.subdomains[subdomainIdx]?.getD "a"
  -- Replace placeholders in URL template
  provider.urlTemplate
    |>.replace "{s}" subdomain
    |>.replace "{z}" (toString tile.z)
    |>.replace "{x}" (toString tile.x)
    |>.replace "{y}" (toString tile.y)

/-- Check if a zoom level is valid for this provider -/
def isValidZoom (provider : TileProvider) (zoom : Int) : Bool :=
  zoom >= provider.minZoom && zoom <= provider.maxZoom

/-- Clamp zoom to provider's valid range -/
def clampZoom (provider : TileProvider) (zoom : Int) : Int :=
  intClamp zoom provider.minZoom provider.maxZoom

/-- Get a unique identifier for cache directory naming -/
def cacheId (provider : TileProvider) : String :=
  -- Create a filesystem-safe identifier from the name
  provider.name.toLower
    |>.replace " " "-"
    |>.replace "@" ""

-- ============================================================================
-- Preset Providers
-- ============================================================================

/-- CartoDB Dark Matter @2x (512px retina tiles) - Default -/
def cartoDarkRetina : TileProvider := {
  name := "CartoDB Dark @2x"
  urlTemplate := "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}@2x.png"
  subdomains := #["a", "b", "c", "d"]
  tileSize := 512
  attribution := "© OpenStreetMap contributors, © CARTO"
  maxZoom := 19
}

/-- CartoDB Dark Matter (256px standard tiles) -/
def cartoDark : TileProvider := {
  name := "CartoDB Dark"
  urlTemplate := "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"
  subdomains := #["a", "b", "c", "d"]
  tileSize := 256
  attribution := "© OpenStreetMap contributors, © CARTO"
  maxZoom := 19
}

/-- CartoDB Positron (Light) @2x -/
def cartoLightRetina : TileProvider := {
  name := "CartoDB Light @2x"
  urlTemplate := "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png"
  subdomains := #["a", "b", "c", "d"]
  tileSize := 512
  attribution := "© OpenStreetMap contributors, © CARTO"
  maxZoom := 19
}

/-- CartoDB Positron (Light) -/
def cartoLight : TileProvider := {
  name := "CartoDB Light"
  urlTemplate := "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"
  subdomains := #["a", "b", "c", "d"]
  tileSize := 256
  attribution := "© OpenStreetMap contributors, © CARTO"
  maxZoom := 19
}

/-- CartoDB Voyager @2x -/
def cartoVoyagerRetina : TileProvider := {
  name := "CartoDB Voyager @2x"
  urlTemplate := "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}@2x.png"
  subdomains := #["a", "b", "c", "d"]
  tileSize := 512
  attribution := "© OpenStreetMap contributors, © CARTO"
  maxZoom := 19
}

/-- CartoDB Voyager -/
def cartoVoyager : TileProvider := {
  name := "CartoDB Voyager"
  urlTemplate := "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png"
  subdomains := #["a", "b", "c", "d"]
  tileSize := 256
  attribution := "© OpenStreetMap contributors, © CARTO"
  maxZoom := 19
}

/-- OpenStreetMap Standard -/
def openStreetMap : TileProvider := {
  name := "OpenStreetMap"
  urlTemplate := "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
  subdomains := #["a", "b", "c"]
  tileSize := 256
  attribution := "© OpenStreetMap contributors"
  maxZoom := 19
}

/-- Stadia Stamen Toner -/
def stamenToner : TileProvider := {
  name := "Stamen Toner"
  urlTemplate := "https://tiles.stadiamaps.com/tiles/stamen_toner/{z}/{x}/{y}.png"
  subdomains := #[]  -- No subdomains
  tileSize := 256
  attribution := "© Stadia Maps, © Stamen Design, © OpenStreetMap contributors"
  maxZoom := 18
}

/-- Stadia Stamen Toner Lite -/
def stamenTonerLite : TileProvider := {
  name := "Stamen Toner Lite"
  urlTemplate := "https://tiles.stadiamaps.com/tiles/stamen_toner_lite/{z}/{x}/{y}.png"
  subdomains := #[]
  tileSize := 256
  attribution := "© Stadia Maps, © Stamen Design, © OpenStreetMap contributors"
  maxZoom := 18
}

/-- Stadia Stamen Terrain -/
def stamenTerrain : TileProvider := {
  name := "Stamen Terrain"
  urlTemplate := "https://tiles.stadiamaps.com/tiles/stamen_terrain/{z}/{x}/{y}.png"
  subdomains := #[]
  tileSize := 256
  attribution := "© Stadia Maps, © Stamen Design, © OpenStreetMap contributors"
  maxZoom := 18
}

/-- Stadia Stamen Watercolor -/
def stamenWatercolor : TileProvider := {
  name := "Stamen Watercolor"
  urlTemplate := "https://tiles.stadiamaps.com/tiles/stamen_watercolor/{z}/{x}/{y}.jpg"
  subdomains := #[]
  tileSize := 256
  attribution := "© Stadia Maps, © Stamen Design, © OpenStreetMap contributors"
  maxZoom := 16
}

/-- Stadia Alidade Smooth (modern light style) -/
def stadiaSmooth : TileProvider := {
  name := "Stadia Smooth"
  urlTemplate := "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}.png"
  subdomains := #[]
  tileSize := 256
  attribution := "© Stadia Maps, © OpenStreetMap contributors"
  maxZoom := 20
}

/-- Stadia Alidade Smooth Dark -/
def stadiaSmoothDark : TileProvider := {
  name := "Stadia Smooth Dark"
  urlTemplate := "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}.png"
  subdomains := #[]
  tileSize := 256
  attribution := "© Stadia Maps, © OpenStreetMap contributors"
  maxZoom := 20
}

/-- Create a custom tile provider from a URL template -/
def custom (name : String) (urlTemplate : String)
    (tileSize : Int := 256) (maxZoom : Int := 19) : TileProvider := {
  name := name
  urlTemplate := urlTemplate
  subdomains := #[]  -- Custom providers typically don't use subdomains
  tileSize := tileSize
  attribution := ""
  maxZoom := maxZoom
}

/-- Default provider (CartoDB Dark @2x for retina displays) -/
def default : TileProvider := cartoDarkRetina

/-- List of all preset providers for UI selection -/
def presets : Array TileProvider := #[
  cartoDarkRetina,
  cartoDark,
  cartoLightRetina,
  cartoLight,
  cartoVoyagerRetina,
  cartoVoyager,
  openStreetMap,
  stamenToner,
  stamenTonerLite,
  stamenTerrain,
  stamenWatercolor,
  stadiaSmooth,
  stadiaSmoothDark
]

end TileProvider

end Tileset
