module Pasta.Render.RawHtmlEl where

import Prelude

import Data.Foldable (foldlDefault)
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
import Pasta.Element (HtmlContainerEl, HtmlEl(..), HtmlVoidEl, children)
import Pasta.Render.Class (class Render, render)

-- | An intermediary type between 'Component' and HTML string.
data RawHtmlEl
  = RawHtmlContainerEl (HtmlContainerEl RawHtmlEl)
  | RawHtmlInner String
  | RawHtmlVoidEl HtmlVoidEl

-- | Set of types that can be converted to the intermediary 'RawHtmlEl' type.
class ToRawHtmlEl a s where
  toRawHtmlEl :: a -> s -> SetState s -> RawHtmlEl

instance Render RawHtmlEl where
  render (RawHtmlContainerEl container) = render container
  render (RawHtmlInner string) = string
  render (RawHtmlVoidEl void) = render void

instance ToRawHtmlEl (ChildComponent s) s where
  toRawHtmlEl child' s setS = runExists toRawHtmlEl' child'
    where
    toRawHtmlEl' :: forall t. ChildComponentF s t -> RawHtmlEl
    toRawHtmlEl' (ChildComponent (sToT /\ updateTInS /\ componentT)) =
      toRawHtmlEl componentT (sToT s) $ \t -> setS $ updateTInS t s

instance ToRawHtmlEl (Component s) s where
  toRawHtmlEl (Component component') s setS =
    toRawHtmlEl (component'.node s setS) s setS

instance ToRawHtmlEl (HtmlEl (Node s)) s where
  toRawHtmlEl (HtmlContainerEl container _) s setS = -- TODO don't ignore listeners
    RawHtmlContainerEl $ map (\n -> toRawHtmlEl n s setS) container
  toRawHtmlEl (HtmlInner inner) _ _ = RawHtmlInner inner
  toRawHtmlEl (HtmlVoidEl void _) _ _ = RawHtmlVoidEl void -- TODO don't ignore listeners

instance ToRawHtmlEl (Node s) s where
  toRawHtmlEl (NodeChildComponent child') s setS = toRawHtmlEl child' s setS
  toRawHtmlEl (NodeHtmlEl html) s setS = toRawHtmlEl html s setS

-- Cache.

type StateHash = Int

type ComponentRef = Component.Key /\ StateHash

type HtmlElOrRef = Either RawHtmlEl ComponentRef

type Cache = Map ComponentRef Flat

data Flat
  = FlatComponentRef ComponentRef
  | FlatHtmlEl (HtmlEl Flat)

-- newtype Identity = Identity (HtmlEl Identity)
-- type RawHtml = HtmlEl Identity

flatChild :: forall s. Cache -> ChildComponent s -> s -> SetState s -> Flat /\ Cache
flatChild cache child' s setS = runExists cacheChild' child'
  where
  cacheChild' :: forall t. ChildComponentF s t -> Flat /\ Cache
  cacheChild' (ChildComponent (sToT /\ updateTInS /\ componentT)) =
    flatComponent cache componentT (sToT s) $ \t -> setS $ updateTInS t s

flatComponent :: forall s. Cache -> Component s -> s -> SetState s -> Flat /\ Cache
flatComponent cache (Component c) s setS = do
  let flatNode' /\ cacheNode = flatNode cache (c.node s setS) s setS
  case c.options.key of
    Nothing -> flatNode' /\ cacheNode
    Just key -> flatNode' /\ Map.insert (key /\ c.options.hash s) flatNode' cacheNode

flatHtmlEl :: forall s. Cache -> HtmlEl (Node s) -> s -> SetState s -> Flat /\ Cache
flatHtmlEl cache (HtmlVoidEl void ls) s setS = FlatHtmlEl (HtmlVoidEl void ls) /\ cache
flatHtmlEl cache (HtmlInner inner) s setS = FlatHtmlEl (HtmlInner inner) /\ cache
flatHtmlEl cache (HtmlContainerEl container ls) s setS =
  let
    flatChildren = map (\node -> flatNode cache node s setS) container
  in
    FlatHtmlEl (HtmlContainerEl (map fst flatChildren) ls)
      /\ foldlDefault Map.union cache (map snd $ children flatChildren)

flatNode :: forall s. Cache -> Node s -> s -> SetState s -> Flat /\ Cache
flatNode cache (NodeChildComponent child') s setS = flatChild cache child' s setS
flatNode cache (NodeHtmlEl htmlEl) s setS = flatHtmlEl cache htmlEl s setS
