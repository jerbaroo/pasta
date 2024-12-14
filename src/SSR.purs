module SSR where

import Prelude (Unit, (>>=))

import Effect (Effect)
import Pasta.Cook as Pasta
import Main (parent)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile, writeTextFile)

main :: Effect Unit
main = do
  readTextFile UTF8 "./index-ssr.html"
    >>= Pasta.prepareDefault "my_app" parent { foo: 1, bar: 2 }
    >>= writeTextFile UTF8 "./index-ssr.out.html"
