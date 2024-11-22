module Pasta.Run where

import Prelude (Unit, bind, discard, show, ($), (<>))

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

import Pasta.Component (Component)
import Pasta.Render.Class (render)
import Pasta.Render.RawHtmlEl (Cache)
import Pasta.Render.RawHtmlEl as Flat

foreign import attach :: String -> String -> Effect Unit
foreign import getCache :: Effect Cache
foreign import setCache :: Cache -> Effect Unit

noCache :: String -> Flat.Strategy Flat.Flat Flat.Cache
noCache attachId = Flat.Strategy
  { getCache: getCache
  , instructions: \c f -> case Flat.flatToRaw c f of
      Left (key /\ hash) -> Left $
        "error: could not find " <> key <> "-" <> show hash <> " in cache"
      Right raw -> Right $ Flat.Attach raw attachId
  , run: Flat.run @Flat.Flat
  , setCache: setCache
  }

run :: forall o c s. Flat.Strategy o c -> c -> Component s -> s -> Effect Unit
run (Flat.Strategy strat) cache0 component s0 = do
  let
    setState :: s -> Effect Unit
    setState sn = do
      currCache <- strat.getCache
      let out /\ nextCache = strat.run currCache component sn setState
      case strat.instructions nextCache out of
        Left error -> log error
        Right (Flat.Attach raw attachId) -> do
          attach attachId $ render raw
          strat.setCache nextCache
  strat.setCache cache0
  setState s0
