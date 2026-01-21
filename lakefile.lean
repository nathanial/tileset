import Lake
open Lake DSL System

package tileset where
  version := v!"0.1.0"
  precompileModules := true

-- Dependencies (using local paths for development)
require cellar from "../cellar"
require wisp from "../../network/wisp"
require raster from "../../graphics/raster"
require reactive from "../reactive"
require crucible from "../../testing/crucible"

-- curl link args (inherited from wisp for HTTP client)
def curlLinkArgs : Array String :=
  if Platform.isOSX then
    #["-L/opt/homebrew/opt/curl/lib",
      "-L/opt/homebrew/lib",
      "-L/usr/local/lib",
      "-L/opt/homebrew/anaconda3/lib",
      "-lcurl",
      "-Wl,-rpath,/opt/homebrew/opt/curl/lib",
      "-Wl,-rpath,/opt/homebrew/lib",
      "-Wl,-rpath,/opt/homebrew/anaconda3/lib",
      "-Wl,-rpath,/usr/local/lib"]
  else if Platform.isWindows then
    #["-lcurl"]
  else
    #["-lcurl", "-Wl,-rpath,/usr/lib", "-Wl,-rpath,/usr/local/lib"]

@[default_target]
lean_lib Tileset where
  roots := #[`Tileset]
  moreLinkArgs := curlLinkArgs

lean_lib TilesetTests where
  roots := #[`TilesetTests]

@[test_driver]
lean_exe tileset_tests where
  root := `TilesetTests.Main
  moreLinkArgs := curlLinkArgs
