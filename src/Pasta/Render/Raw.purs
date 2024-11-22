module Pasta.Render.Raw where

import Pasta.Element (HtmlEl(..))
import Pasta.Render.Class (class Render, render)

newtype Raw = Raw (HtmlEl Raw)

instance Render Raw where
  render (Raw (HtmlContainerEl container)) = render container
  render (Raw (HtmlInner string)) = string
  render (Raw (HtmlVoidEl void _)) = render void
