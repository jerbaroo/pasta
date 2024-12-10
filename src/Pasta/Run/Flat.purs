module Pasta.Run.Flat where

import Prelude

import Data.Foldable (foldlDefault)
import Data.Exists (runExists)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)

import Pasta.Component as Component
import Pasta.Component (ChildComponent, ChildComponentF(..), Component(..), Node(..), UpdateState)
import Pasta.Element (Container, Element(..))
import Pasta.Element as Element
import Pasta.Listener (Handler, Listener)
import Pasta.Run.Class (class Run, run)

-- * VDom.

type StateHash = Int

type ComponentRef = Component.Key /\ StateHash

data VDom = VDom
  { components :: Map ComponentRef Flat
  , globalFuncs :: Map String Handler
  , nextFuncId :: Int
  }

addGlobalFuncs :: Int -> Array (String /\ Handler) -> VDom -> VDom
addGlobalFuncs nextFuncId funcs (VDom v) = VDom v
  { globalFuncs = Map.union v.globalFuncs $ Map.fromFoldable funcs
  , nextFuncId = nextFuncId
  }

insert :: ComponentRef -> Flat -> VDom -> VDom
insert key val (VDom v) =
  VDom v { components = Map.insert key val v.components }

union :: VDom -> VDom -> VDom
union (VDom v1) (VDom v2) = VDom
  { components: Map.union v1.components v2.components
  , globalFuncs: Map.union v1.globalFuncs v2.globalFuncs
  , nextFuncId: max v1.nextFuncId v2.nextFuncId
  }

-- * Flat.

data Flat
  = FlatComponentRef ComponentRef
  | FlatElement (Element Void Flat)

instance Run (ChildComponent s) Flat VDom s where
  run globalId vDom child s updateS = runExists runChild child
   where
    runChild :: forall t. ChildComponentF s t -> Effect (Flat /\ VDom)
    runChild (ChildComponent (sToT /\ setTInS /\ componentT)) =
      run globalId vDom componentT (sToT s) \f ->
        updateS \s' -> setTInS (f $ sToT s') s'

instance Run (Component s) Flat VDom s where
  run = runComponent

runComponent :: forall s.
  String -> VDom -> Component s -> s -> UpdateState s -> Effect (Flat /\ VDom)
runComponent globalId vDom (Component c) s updateS = do
  c.options.onUpdate s
  flatNode /\ vDomNode <- run globalId vDom (c.node s updateS) s updateS
  pure $ flatNode /\ case c.options.key of
    Nothing  -> vDomNode
    Just key -> insert (key /\ c.options.hash s) flatNode vDomNode

instance Run (Element Listener (Node s)) Flat VDom s where
  run globalId vDom@(VDom v) (ElementContainer container) s updateS = do
    -- Apply 'run' to each child in the container.
    (container' :: Container Listener (Flat /\ VDom)) <- Element.sequence $
      container <#> \node -> run globalId vDom node s updateS
    -- Apply listeners to element as attribtues e.g. "onclick".
    let (nextFuncId /\ ls /\ container'') =
          Element.listenersToAttrs globalId v.nextFuncId container'
    let newVDom = addGlobalFuncs nextFuncId ls $ foldlDefault union vDom $
          snd <$> Element.children container''
    pure $ FlatElement (ElementContainer $ fst <$> container'') /\ newVDom

  run _ vDom (ElementInner inner) _ _ =
    pure $ FlatElement (ElementInner inner) /\ vDom

  run globalId vDom@(VDom v) (ElementVoid void) _ _ = pure $
    let (nextFuncId /\ ls /\ void') = Element.listenersToAttrs globalId v.nextFuncId void
    in  FlatElement (ElementVoid void') /\ (addGlobalFuncs nextFuncId ls vDom)

instance Run (Node s) Flat VDom s where
  run globalId vDom (NodeChildComponent child) = run globalId vDom child
  run globalId vDom (NodeElement element) = run globalId vDom element
