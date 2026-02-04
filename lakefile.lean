import Lake
open Lake DSL System

package tileset where
  version := v!"0.1.0"
  precompileModules := true

require cellar from git "https://github.com/nathanial/cellar" @ "v0.0.2"
require wisp from git "https://github.com/nathanial/wisp" @ "v0.0.2"
require raster from git "https://github.com/nathanial/raster" @ "v0.0.5"
require reactive from git "https://github.com/nathanial/reactive" @ "v0.2.2"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.10"

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
