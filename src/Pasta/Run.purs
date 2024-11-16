module Pasta.Run where

import Prelude (Unit, bind, discard, show, (<>))

import Data.Either (Either(..))
import Data.Map (empty)
import Data.Tuple.Nested ((/\))
import Effect (Effect)

import Pasta.Component (Component(..))
import Pasta.Render.Class (render)
import Pasta.Render.RawHtmlEl (Cache, flatComponent, flatToRaw)

foreign import attach :: String -> String -> Effect Unit
foreign import getCache :: Effect Cache
foreign import setCache :: Cache -> Effect Unit

run :: forall s. String -> Component s -> s -> Effect Unit
run attachId component@(Component c) s0 = do
  let
    setState sn = do
      c.options.onUpdate sn
      cache <- getCache
      let flat /\ cacheNext = flatComponent cache component sn setState
      let
        html = case flatToRaw cacheNext flat of
          Left (key /\ hash) ->
            "error: could not find " <> key <> "-" <> show hash <> " in cache"
          Right raw -> render raw
      -- ..and attach to the DOM.
      attach attachId html
      setCache cacheNext
  setCache empty
  setState s0
