module Pasta.Cook where

import Prelude (Unit, bind, discard, identity, ($), (<>))

import Data.Either (Either(..))
import Data.Functor ((<#>))
import Data.Tuple.Nested ((/\))
import Effect (Effect)

import Pasta.Component (Component)
import Pasta.Render.Class (render)
import Pasta.Strategy (Strategy, DomUpdates(..))

foreign import attach :: String -> String -> Effect Unit

foreign import getGlobal :: forall a. String -> Effect a
foreign import setGlobal :: forall a. String -> a -> Effect Unit

cook :: forall o c s. String -> Strategy o c -> c -> Component s -> s -> Effect Unit
cook globalId strat vDom0 component s0 = do
  let getState = getGlobal $ globalId <> "-state"
  let setState = setGlobal $ globalId <> "-state"
  let getVDom = getGlobal $ globalId <> "-vdom"
  let setVDom = setGlobal $ globalId <> "-vdom"
  let
    updateState :: (s -> s) -> Effect Unit
    updateState update = do
      vDomOld <- getVDom
      state <- getState <#> update
      root /\ vDomNew <- strat.run vDomOld component state updateState
      case strat.instructions vDomOld vDomNew root of
        Left error -> strat.onError error
        Right (InnerHtml raw attachId) -> do
          attach attachId $ render raw
          setGlobal globalId vDomNew
  setState s0
  setVDom vDom0
  updateState identity
