module Pasta.Render.RawHtmlEl where

import Prelude

import Data.Foldable (foldlDefault)
import Data.Functor ((<#>))
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Hashable (class Hashable, hash)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

import Pasta.Component as Component
import Pasta.Component (ChildComponent, ChildComponentF(..), Component(..), Node(..), SetState)
import Pasta.Element (ContainerEl(..), HtmlEl(..), HtmlVoidEl, children, sequence)

import Pasta.Render.Class (class Render, render)

type StateHash = Int

type ComponentRef = Component.Key /\ StateHash

type Cache = Map ComponentRef Flat

data Flat
  = FlatComponentRef ComponentRef
  | FlatHtmlEl (HtmlEl Flat)

flatChild :: forall s. Cache -> ChildComponent s -> s -> SetState s -> Flat /\ Cache
flatChild cache child' s setS = runExists cacheChild' child'
  where
  cacheChild' :: forall t. ChildComponentF s t -> Flat /\ Cache
  cacheChild' (ChildComponent (sToT /\ updateTInS /\ componentT)) =
    flatComponent cache componentT (sToT s) $ \t -> setS $ updateTInS t s

flatComponent :: forall s. Cache -> Component s -> s -> SetState s -> Flat /\ Cache
flatComponent cache (Component c) s setS = do
  let flatNode' /\ cacheNode = flatNode cache (c.node s setS) s setS
  flatNode' /\ case c.options.key of
    Nothing -> cacheNode
    Just key -> Map.insert (key /\ c.options.hash s) flatNode' cacheNode

flatHtmlEl :: forall s. Cache -> HtmlEl (Node s) -> s -> SetState s -> Flat /\ Cache
flatHtmlEl cache (HtmlContainerEl container) s setS =
  let
    flatChildren = map (\node -> flatNode cache node s setS) container
  in
    FlatHtmlEl (HtmlContainerEl $ map fst flatChildren)
      /\ foldlDefault Map.union cache (map snd $ children flatChildren)
flatHtmlEl cache (HtmlInner inner) s setS = FlatHtmlEl (HtmlInner inner) /\ cache
flatHtmlEl cache (HtmlVoidEl void ls) s setS = FlatHtmlEl (HtmlVoidEl void ls) /\ cache

flatNode :: forall s. Cache -> Node s -> s -> SetState s -> Flat /\ Cache
flatNode cache (NodeChildComponent child') s setS = flatChild cache child' s setS
flatNode cache (NodeHtmlEl htmlEl) s setS = flatHtmlEl cache htmlEl s setS

newtype Raw = Raw (HtmlEl Raw)

flatToRaw :: Cache -> Flat -> Either ComponentRef Raw
flatToRaw cache (FlatComponentRef ref) =
  case Map.lookup ref cache of
    Nothing -> Left ref
    Just flat -> flatToRaw cache flat
flatToRaw cache (FlatHtmlEl (HtmlContainerEl container)) =
  sequence (map (flatToRaw cache) container) <#> Raw <<< HtmlContainerEl
flatToRaw _ (FlatHtmlEl (HtmlInner inner)) = Right $ Raw $ HtmlInner inner
flatToRaw _ (FlatHtmlEl (HtmlVoidEl void ls)) = Right $ Raw $ HtmlVoidEl void ls

instance Render Raw where
  render (Raw (HtmlContainerEl container)) = render container
  render (Raw (HtmlInner string)) = string
  render (Raw (HtmlVoidEl void _)) = render void
