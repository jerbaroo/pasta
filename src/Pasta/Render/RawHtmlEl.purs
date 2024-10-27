module Pasta.Render.RawHtmlEl where

import Prelude

import Data.Exists (runExists)
import Data.Tuple.Nested ((/\))

import Pasta.Component (ChildComponent, ChildComponentF(..), Component, Node(..), SetState)
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
        renderComponent componentT (sToT s) $ \t -> setS $ updateTInS t s

renderComponent :: forall s. Component s -> s -> SetState s -> RawHtmlEl
renderComponent component' s setS = renderNode (component'.node s setS) s setS

renderHtml :: forall s. HtmlEl (Node s) -> s -> SetState s -> RawHtmlEl
renderHtml (HtmlContainerEl container) s setS =
  RawHtmlContainerEl $ map (\n -> renderNode n s setS) container
renderHtml (HtmlInner inner) _ _ = RawHtmlInner inner
renderHtml (HtmlVoidEl void) _ _ = RawHtmlVoidEl void

renderNode :: forall s. Node s -> s -> SetState s -> RawHtmlEl
renderNode (NodeChildComponent child') s setS = toRawHtmlEl child' s setS
renderNode (NodeHtmlEl html) s setS = renderHtml html s setS
