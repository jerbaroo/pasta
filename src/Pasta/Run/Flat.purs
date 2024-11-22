module Pasta.Run.Flat where

import Prelude

import Data.Foldable (foldlDefault)
import Data.Exists (runExists)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

import Pasta.Component as Component
import Pasta.Component (ChildComponent, ChildComponentF(..), Component(..), Node(..), SetState)
import Pasta.Element (Element(..))
import Pasta.Element as Element
import Pasta.Run.Class (class Run, run)

type StateHash = Int

type ComponentRef = Component.Key /\ StateHash

-- TODO rename to VDom and include global listeners.
type Cache = Map ComponentRef Flat

data Flat
  = FlatComponentRef ComponentRef
  | FlatElement (Element Flat)

instance Run Flat (ChildComponent s) Cache s where
  run c child' s setS = runExists cacheChild' child'
    where
    cacheChild' :: forall t. ChildComponentF s t -> Flat /\ Cache
    cacheChild' (ChildComponent (sToT /\ updateTInS /\ componentT)) =
      run c componentT (sToT s) $ \t -> setS $ updateTInS t s

instance Run Flat (Component s) Cache s where
  run = runComponent

runComponent :: forall s. Cache -> Component s -> s -> SetState s -> Flat /\ Cache
runComponent c (Component comp) s setS = do
  let
    node = comp.node s $ \sNew -> do
      comp.options.onUpdate sNew
      setS sNew
  let flatNode /\ cacheNode = run c node s setS
  flatNode /\ case comp.options.key of
    Nothing -> cacheNode
    Just key -> Map.insert (key /\ comp.options.hash s) flatNode cacheNode

instance Run Flat (Element (Node s)) Cache s where
  run c (ElementContainer container) s setS =
    let
      flatChildren = map (\node -> run c node s setS) container
    in
      FlatElement (ElementContainer $ map fst flatChildren)
        /\ foldlDefault Map.union c (map snd $ Element.children flatChildren)
  run c (ElementInner inner) _ _ = FlatElement (ElementInner inner) /\ c
  run c (ElementVoid void) _ _ = FlatElement (ElementVoid void) /\ c

instance Run Flat (Node s) Cache s where
  run c (NodeChildComponent child') = run c child'
  run c (NodeElement element) = run c element
