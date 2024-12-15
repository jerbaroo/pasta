module Pasta.Cook where

import Prelude (Unit, bind, const, discard, identity, pure, unit, ($), (<>))

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function (flip)
import Data.Functor ((<#>))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Exception (throw)

import Pasta.Component (class SSRState, Component, showStateUnsafe)
import Pasta.Listener (Handler)
import Pasta.Render.Class (render)
import Pasta.Run.Flat (Flat, VDom) as Flat
import Pasta.Strategy (Strategy, DomUpdates(..))
import Pasta.Strategy.Flat (emptyVDom, innerHtml) as Flat

-- * Internal functions for global Pasta state.

-- | Get a global (within pasta namespace) variable.
foreign import getGlobal :: forall a. String -> Effect a

-- | attach _ a b == document.getElementById(a).innerHTML = b
foreign import innerHTML :: String -> String -> String -> Effect Unit

-- | Set a global (within pasta namespace) variable.
foreign import setGlobal :: forall a. String -> a -> Effect Unit

-- | Register a global function.
foreign import registerFunc :: String -> Handler -> Effect Unit

-- * Internal foreign functions for SSR.

-- | Set the SSR state for server-side rendering.
foreign import getSSRState :: forall a.
  String -> Maybe a -> (a -> Maybe a) -> Effect (Maybe a)

-- | Attach the HTML body during server-side rendering.
foreign import innerHTMLSSR :: String -> String -> String

-- | Cleanup any remaining traces from SSR modifications.
foreign import postSSRCleanup :: String -> String

-- | Register a global function during server-side rendering.
foreign import registerFuncSSR :: String -> String -> String

-- | Set the SSR state for server-side rendering.
foreign import setSSRState :: String -> String -> String -> String

-- * Default Strategy and VDom.

defaultStrategy :: String -> Strategy Flat.Flat Flat.VDom
defaultStrategy = Flat.innerHtml

defaultVDom :: Flat.VDom
defaultVDom = Flat.emptyVDom

-- * Entry points for Pasta apps.

cookDefault :: forall s.
  String -> String -> Component s -> Maybe s -> Effect Unit
cookDefault appName root = cook appName (defaultStrategy root) defaultVDom

cook :: forall o c s.
  String -> Strategy o c -> c -> Component s -> Maybe s -> Effect Unit
cook appName strat vDom0 component s0May = do
  let getState = getGlobal $ appName <> "-state"
  let setState = setGlobal $ appName <> "-state"
  let getVDom = getGlobal $ appName <> "-vdom"
  let setVDom = setGlobal $ appName <> "-vdom"
  let msg s = "PASTA (" <> appName <> "): " <> s

  let
    updateState :: Boolean -> (s -> s) -> Effect Unit
    updateState ssrFirstRender update = do
      vDomOld         <- getVDom
      state           <- getState <#> update
      let root /\ vDomNew =
            strat.run appName vDomOld component state $ updateState false
      case strat.instructions vDomOld vDomNew root of
        Left  error   -> strat.onError error
        Right updates -> flip traverse_ updates $ case _ of
          (RegisterFunc name func) -> do
            registerFunc name func
            log $ msg $ "Registered function " <> name
          (InnerHtml raw attachId) -> do
            if   ssrFirstRender
            then log $ msg $ "Not attaching body via innerHTML due to SSR"
            else innerHTML appName attachId $ render raw
            setGlobal appName vDomNew
            log $ msg $ "Saved virtual dom"

  -- Determine app's initial state, either via SSR or provided.
  ssr0May      <- getSSRState appName Nothing Just
  initialState <- case (s0May /\ ssr0May) of
    (Just _ /\ Just ssr0) -> do
      error $ msg "State passed to cook will be overriden by SSR"
      pure ssr0
    (Just s0 /\ Nothing) -> pure s0
    (Nothing /\ Just ssr0) -> pure ssr0
    (Nothing /\ Nothing) -> throw $ msg "State must either be passed to 'cook' or via SSR"

  -- Save app's state globally.
  setState initialState
  -- Save app's VDom globally.
  setVDom vDom0
  -- Render once.
  updateState (isJust ssr0May) identity

-- * Server-side rendering.

prepareDefault :: forall s. SSRState s =>
  String -> Component s -> s -> String -> Effect String
prepareDefault appName = prepare appName (defaultStrategy "") defaultVDom

prepare :: forall o c s. SSRState s =>
  String -> Strategy o c -> c -> Component s -> s -> String -> Effect String
prepare appName strat vDom0 component s0 html0 = do
  let root /\ vDomNew =
        strat.run appName vDom0 component s0 $ const $ pure unit
  case strat.instructions vDom0 vDomNew root of
    Left  error   -> throw error
    Right updates -> do
      let f html (RegisterFunc name _) = registerFuncSSR name html
          f html (InnerHtml    raw  _) = innerHTMLSSR (render raw) html
      pure $ postSSRCleanup $ setSSRState appName (showStateUnsafe s0) $
        Array.foldl f html0 updates
