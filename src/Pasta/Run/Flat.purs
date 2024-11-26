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
import Pasta.Element (Element(..))
import Pasta.Element as Element
import Pasta.Run.Class (class Run, run)

-- * VDom.

type StateHash = Int

type ComponentRef = Component.Key /\ StateHash

data VDom = VDom
  { components :: Map ComponentRef Flat -- TODO include HTML element ID
  , listeners :: Map Int (Effect Unit)
  , nextListenerId :: Int
  }

addListener :: Effect Unit -> VDom -> VDom
addListener f (VDom v) = VDom v
  { listeners = Map.insert v.nextListenerId f v.listeners
  , nextListenerId = v.nextListenerId + 1
  }

insert :: ComponentRef -> Flat -> VDom -> VDom
insert key val (VDom v) =
  VDom v { components = Map.insert key val v.components }

union :: VDom -> VDom -> VDom
union (VDom v1) (VDom v2) = VDom
  { components: Map.union v1.components v2.components
  , listeners: Map.union v1.listeners v2.listeners
  , nextListenerId: max v1.nextListenerId v2.nextListenerId
  }

-- * Flat.

data Flat
  = FlatComponentRef ComponentRef
  | FlatElement (Element Flat)

instance Run (ChildComponent s) Flat VDom s where
  run vDom child s updateS = runExists runChild child
    where
    runChild :: forall t. ChildComponentF s t -> Effect (Flat /\ VDom)
    runChild (ChildComponent (sToT /\ setTInS /\ componentT)) =
      run vDom componentT (sToT s) \f -> updateS \s' -> setTInS (f $ sToT s') s'

instance Run (Component s) Flat VDom s where
  run = runComponent

runComponent :: forall s. VDom -> Component s -> s -> UpdateState s -> Effect (Flat /\ VDom)
runComponent vDom (Component comp) s updateS = do
  comp.options.onUpdate s
  let node = comp.node s updateS
  flatNode /\ vDomNode <- run vDom node s updateS
  pure $ flatNode /\ case comp.options.key of
    Nothing -> vDomNode
    Just key -> insert (key /\ comp.options.hash s) flatNode vDomNode

instance Run (Element (Node s)) Flat VDom s where
  run vDom (ElementContainer container) s updateS = do
    runChildren <- Element.sequence $ container <#> \node -> run vDom node s updateS
    pure $ FlatElement (ElementContainer $ fst <$> runChildren)
      /\ foldlDefault union vDom (snd <$> Element.children runChildren)
  run vDom (ElementInner inner) _ _ = pure $ FlatElement (ElementInner inner) /\ vDom
  run vDom (ElementVoid void) _ _ = pure $ FlatElement (ElementVoid void) /\ vDom

instance Run (Node s) Flat VDom s where
  run vDom (NodeChildComponent child) = run vDom child
  run vDom (NodeElement element) = run vDom element
