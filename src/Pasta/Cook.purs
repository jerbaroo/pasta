module Pasta.Cook where

import Prelude (Unit, bind, const, discard, identity, pure, unit, ($), (<>))

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function (flip)
import Data.Functor ((<#>))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)

import Pasta.Component (class SSRState, Component)
import Pasta.Listener (Handler)
import Pasta.Render.Class (render)
import Pasta.Run.Flat (Flat, VDom) as Flat
import Pasta.Strategy (Strategy, DomUpdates(..))
import Pasta.Strategy.Flat (emptyVDom, innerHtml) as Flat

-- | attach a b == document.getElementById(a).innerHTML = b
foreign import attach :: String -> String -> Effect Unit

-- | Attach the HTML body during server-side rendering.
foreign import attachSSR :: String -> String -> String

-- | Get a global (within pasta namespace) variable.
foreign import getGlobal :: forall a. String -> Effect a

-- | Set a global (within pasta namespace) variable.
foreign import setGlobal :: forall a. String -> a -> Effect Unit

foreign import postSSRCleanup :: String -> String

-- | Register a global function.
foreign import registerFunc :: String -> Handler -> Effect Unit

-- | Register a global function during server-side rendering.
foreign import registerFuncSSR :: String -> String -> String

defaultStrategy :: String -> Strategy Flat.Flat Flat.VDom
defaultStrategy = Flat.innerHtml

defaultVDom :: Flat.VDom
defaultVDom = Flat.emptyVDom

cookDefault :: forall s. String -> String -> Component s -> s -> Effect Unit
cookDefault globalId root = cook globalId (defaultStrategy root) defaultVDom

cook :: forall o c s.
  String -> Strategy o c -> c -> Component s -> s -> Effect Unit
cook globalId strat vDom0 component s0 = do
  let getState = getGlobal $ globalId <> "-state"
  let setState = setGlobal $ globalId <> "-state"
  let getVDom = getGlobal $ globalId <> "-vdom"
  let setVDom = setGlobal $ globalId <> "-vdom"
  let
    updateState :: (s -> s) -> Effect Unit
    updateState update = do
      vDomOld         <- getVDom
      state           <- getState <#> update
      let root /\ vDomNew =
            strat.run globalId vDomOld component state updateState
      case strat.instructions vDomOld vDomNew root of
        Left  error   -> strat.onError error
        Right updates -> flip traverse_ updates $ case _ of
          (RegisterFunc name func) -> registerFunc name func
          (InnerHtml raw attachId) -> do
            attach attachId $ render raw
            setGlobal globalId vDomNew
  setState s0
  setVDom vDom0
  updateState identity

-- * Server-side rendering.

prepareDefault :: forall s. SSRState s =>
  String -> Component s -> s -> String -> Effect String
prepareDefault globalId = prepare globalId (defaultStrategy "") defaultVDom

prepare :: forall o c s. SSRState s =>
  String -> Strategy o c -> c -> Component s -> s -> String -> Effect String
prepare globalId strat vDom0 component s0 html0 = do
  let root /\ vDomNew =
        strat.run globalId vDom0 component s0 $ const $ pure unit
  case strat.instructions vDom0 vDomNew root of
    Left  error   -> throw error
    Right updates -> do
      let f html (RegisterFunc name _) = registerFuncSSR name html
          f html (InnerHtml    raw  _) = attachSSR (render raw) html
      pure $ postSSRCleanup $ Array.foldl f html0 updates
