module Pasta.Render.RawHtmlEl where

import Prelude

import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Hashable (class Hashable, hash)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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

-- Version 2.

type StateHash = Int

type ComponentRef = Component.Key /\ StateHash

type HtmlElOrRef = Either RawHtmlEl ComponentRef

type Cache = Map ComponentRef (Array HtmlElOrRef)

data Flat
  = FlatComponentRef ComponentRef
  | FlatHtmlEl (HtmlEl Flat)

newtype Identity = Identity (HtmlEl Identity)
type RawHtml = HtmlEl Identity

cacheChild :: forall s. ChildComponent s -> s -> SetState s -> Flat
cacheChild child' s setS = runExists cacheChild' child'
  where
  cacheChild' :: forall t. ChildComponentF s t -> Flat
  cacheChild' (ChildComponent (sToT /\ updateTInS /\ componentT)) =
    cacheComponent componentT (sToT s) $ \t -> setS $ updateTInS t s

cacheComponent :: forall s. Component s -> s -> SetState s -> Flat
cacheComponent (Component c) s setS =
  case c.options.key of
    Nothing -> cacheNode (c.node s setS) s setS
    Just key -> FlatComponentRef $ key /\ c.options.hash s -- TODO cache value

cacheHtmlEl :: forall s. HtmlEl (Node s) -> s -> SetState s -> Flat
cacheHtmlEl (HtmlVoidEl void ls) s setS = FlatHtmlEl $ HtmlVoidEl void ls
cacheHtmlEl (HtmlInner inner) s setS = FlatHtmlEl $ HtmlInner inner
cacheHtmlEl (HtmlContainerEl container ls) s setS = FlatHtmlEl $
    HtmlContainerEl (map (\node -> cacheNode node s setS) container) ls

cacheNode :: forall s. Node s -> s -> SetState s -> Flat
cacheNode (NodeChildComponent child') s setS = cacheChild child' s setS
cacheNode (NodeHtmlEl htmlEl) s setS = cacheHtmlEl htmlEl s setS
