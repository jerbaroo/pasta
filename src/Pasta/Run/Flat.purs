module Pasta.Run.Flat where

import Prelude

import Data.Foldable (foldlDefault)
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

import Pasta.Component as Component
import Pasta.Component (ChildComponent, ChildComponentF(..), Component(..), Node(..), SetState)
import Pasta.Element (HtmlEl(..))
import Pasta.Element as Element
import Pasta.Render.Raw (Raw(..))
import Pasta.Run.Class (class Run, run)

type StateHash = Int

type ComponentRef = Component.Key /\ StateHash

type Cache = Map ComponentRef Flat

data Flat
  = FlatComponentRef ComponentRef
  | FlatHtmlEl (HtmlEl Flat)

instance Run Flat (ChildComponent s) Cache s where
  run c child' s setS = runExists cacheChild' child'
    where
    cacheChild' :: forall t. ChildComponentF s t -> Flat /\ Cache
    cacheChild' (ChildComponent (sToT /\ updateTInS /\ componentT)) =
      -- TODO call onUpdate
      run c componentT (sToT s) $ \t -> setS $ updateTInS t s

instance Run Flat (Component s) Cache s where
  run = runComponent

runComponent :: forall s. Cache -> Component s -> s -> SetState s -> Flat /\ Cache
runComponent c (Component comp) s setS = do
    let flatNode' /\ cacheNode = run c (comp.node s setS) s setS
    flatNode' /\ case comp.options.key of
      Nothing -> cacheNode
      Just key -> Map.insert (key /\ comp.options.hash s) flatNode' cacheNode

instance Run Flat (HtmlEl (Node s)) Cache s where
  run c (HtmlContainerEl container) s setS =
    let
      flatChildren = map (\node -> run c node s setS) container
    in
      FlatHtmlEl (HtmlContainerEl $ map fst flatChildren)
        /\ foldlDefault Map.union c (map snd $ Element.children flatChildren)
  run c (HtmlInner inner) _ _ = FlatHtmlEl (HtmlInner inner) /\ c
  run c (HtmlVoidEl void ls) _ _ = FlatHtmlEl (HtmlVoidEl void ls) /\ c

instance Run Flat (Node s) Cache s where
  run c (NodeChildComponent child') = run c child'
  run c (NodeHtmlEl htmlEl) = run c htmlEl

-- | Convert a 'Flat' to a 'Raw' using a 'Cache'.
-- TODO move to Pasta.Strategy.Flat
flatToRaw :: Cache -> Flat -> Either ComponentRef Raw
flatToRaw c (FlatComponentRef ref) =
  case Map.lookup ref c of
    Nothing -> Left ref
    Just flat -> flatToRaw c flat
flatToRaw c (FlatHtmlEl (HtmlContainerEl container)) =
  Element.sequence (map (flatToRaw c) container) <#> Raw <<< HtmlContainerEl
flatToRaw _ (FlatHtmlEl (HtmlInner inner)) = Right $ Raw $ HtmlInner inner
flatToRaw _ (FlatHtmlEl (HtmlVoidEl void ls)) = Right $ Raw $ HtmlVoidEl void ls
