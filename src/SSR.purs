module SSR where

import Prelude (Unit, (<#>), (>>=))

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect (Effect)
import Effect.Console (error)
import Main (AppState(..), app)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (argv)
import Pasta.Cook as Pasta

exampleState :: AppState
exampleState = AppState { foo: 1, bar: 2 }

main :: Effect Unit
main = (argv <#> \x -> x !! 2) >>= case _ of
  Nothing   -> error "No file path given"
  Just path ->
    readTextFile UTF8 path
    >>= Pasta.prepareDefault "my_app" app exampleState
    >>= writeTextFile UTF8
          (replace (Pattern ".html") (Replacement ".out.html") path)
