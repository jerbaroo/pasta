module Pasta.Cook where

import Prelude (Unit, bind, discard, identity, pure, unit, ($), (<>))

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Functor ((<#>))
import Data.Tuple.Nested ((/\))
import Effect (Effect)

import Pasta.Component (Component)
import Pasta.Listener (Handler)
import Pasta.Render.Class (render)
import Pasta.Strategy (Strategy, DomUpdates(..))
import Pasta.Strategy.Flat as Flat

foreign import attach :: String -> String -> Effect Unit

foreign import getGlobal :: forall a. String -> Effect a
foreign import setGlobal :: forall a. String -> a -> Effect Unit

foreign import registerFunc :: String -> Handler -> Effect Unit

cookDefault :: forall s. String -> String -> Component s -> s -> Effect Unit
cookDefault globalId root = cook globalId (Flat.innerHtml root) Flat.emptyVDom

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
      root /\ vDomNew <- strat.run globalId vDomOld component state updateState
      case strat.instructions vDomOld vDomNew root of
        Left error -> strat.onError error
        Right updates -> do
          let f (InnerHtml raw attachId) = do
                attach attachId $ render raw
                setGlobal globalId vDomNew
              f (RegisterFunc name func) = registerFunc name func
          traverse_ f updates
          pure unit
  setState s0
  setVDom vDom0
  updateState identity
