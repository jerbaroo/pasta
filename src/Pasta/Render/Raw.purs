module Pasta.Render.Raw where

import Prelude (Void)

import Pasta.Element (Element(..))
import Pasta.Render.Class (class Render, render)

newtype Raw = Raw (Element Void Raw)

instance Render Raw where
  render (Raw (ElementContainer container)) = render container
  render (Raw (ElementInner string)) = string
  render (Raw (ElementVoid void)) = render void
