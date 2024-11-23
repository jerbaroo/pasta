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

foreign import getGlobal :: forall a. String -> Effect a
foreign import setGlobal :: forall a. String -> a -> Effect Unit

cook :: forall o c s. String -> Strategy o c -> c -> Component s -> s -> Effect Unit
cook globalId strat vDom0 component s0 = do
  let
    setState :: s -> Effect Unit
    setState sn = do
      vDomOld <- getGlobal globalId
      let root /\ vDomNew = strat.run vDomOld component sn setState
      case strat.instructions vDomOld vDomNew root of
        Left error -> log error
        Right (InnerHtml raw attachId) -> do
          attach attachId $ render raw
          setGlobal globalId vDomNew
  setGlobal globalId vDom0
  setState s0
