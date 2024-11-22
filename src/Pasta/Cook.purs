module Pasta.Cook where

import Prelude (Unit, bind, discard, ($))

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

import Pasta.Component (Component)
import Pasta.Render.Class (render)
import Pasta.Strategy (Strategy, DomUpdates(..))

foreign import attach :: String -> String -> Effect Unit

foreign import getCache :: forall a. String -> Effect a
foreign import setCache :: forall a. String -> a -> Effect Unit

cook :: forall o c s. String -> Strategy o c -> c -> Component s -> s -> Effect Unit
cook cacheId strat cache0 component s0 = do
  let
    setState :: s -> Effect Unit
    setState sn = do
      currCache <- getCache cacheId
      let out /\ nextCache = strat.run currCache component sn setState
      case strat.instructions nextCache out of
        Left error -> log error
        Right (InnerHtml raw attachId) -> do
          attach attachId $ render raw
          setCache cacheId nextCache
  setCache cacheId cache0
  setState s0
