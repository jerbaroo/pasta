module Pasta.Render.RawHtmlEl where

import Prelude

import Data.Exists (runExists)
import Data.Tuple.Nested ((/\))

import Pasta.Component (ChildComponent, ChildComponentF(..), Component(..), Node(..), SetState)
import Pasta.Element (HtmlContainerEl, HtmlEl(..), HtmlVoidEl)
import Pasta.Render.Class (class Render, render)

-- | An intermediary type between 'Component' and HTML string.
data RawHtmlEl
  = RawHtmlContainerEl (HtmlContainerEl RawHtmlEl)
  | RawHtmlInner       String
  | RawHtmlVoidEl      HtmlVoidEl

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
  toRawHtmlEl (HtmlContainerEl container) s setS =
    RawHtmlContainerEl $ map (\n -> toRawHtmlEl n s setS) container
  toRawHtmlEl (HtmlInner inner) _ _ = RawHtmlInner inner
  toRawHtmlEl (HtmlVoidEl void) _ _ = RawHtmlVoidEl void

instance ToRawHtmlEl (Node s) s where
  toRawHtmlEl (NodeChildComponent child') s setS = toRawHtmlEl child' s setS
  toRawHtmlEl (NodeHtmlEl html) s setS = toRawHtmlEl html s setS
