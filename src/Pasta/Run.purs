module Pasta.Run where

import Prelude (Unit, bind, discard, show, ($), (<>))

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

import Pasta.Component (Component)
import Pasta.Render.Class (render)
import Pasta.Run.Flat as Flat
import Pasta.Strategy (Strategy(..), DomUpdates(..))

foreign import attach :: String -> String -> Effect Unit

-- TODO polymorphic
foreign import getCache :: Effect Flat.Cache
foreign import setCache :: Flat.Cache -> Effect Unit

-- TODO move to Pasta.Strategy.Flat
noCache :: String -> Strategy Flat.Flat Flat.Cache
noCache attachId = Strategy
  { getCache: getCache
  , instructions: \c f -> case Flat.flatToRaw c f of
      Left (key /\ hash) -> Left $
        "error: could not find " <> key <> "-" <> show hash <> " in cache"
      Right raw -> Right $ InnerHtml raw attachId
  , run: Flat.runComponent
  , setCache: setCache
  }

run :: forall o c s. Strategy o c -> c -> Component s -> s -> Effect Unit
run (Strategy strat) cache0 component s0 = do
  let
    setState :: s -> Effect Unit
    setState sn = do
      currCache <- strat.getCache
      let out /\ nextCache = strat.run currCache component sn setState
      case strat.instructions nextCache out of
        Left error -> log error
        Right (InnerHtml raw attachId) -> do
          attach attachId $ render raw
          strat.setCache nextCache
  strat.setCache cache0
  setState s0
