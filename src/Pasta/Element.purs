module Pasta.Element where

import Prelude (class Functor, map, ($), (<>))

import Control.Applicative (class Applicative)
import Data.Array (null)
import Data.Foldable (foldMap)
import Data.Functor ((<#>))
import Data.Traversable as Traversable

import Pasta.Attribute (class HasAttrs, Attrs(..), DivAttr, attrs, toGenericAttr)
import Pasta.Listener (class HasListeners, Listener)
import Pasta.Render.Class (class Render, render)

-- * HTML element.

data HtmlEl a
  = HtmlContainerEl (ContainerEl a)
  | HtmlInner String
  | HtmlVoidEl HtmlVoidEl (Array Listener)

instance HasListeners (HtmlEl a) where
  listeners (HtmlContainerEl (ContainerEl _ _ _ ls)) = ls
  listeners (HtmlInner _) = []
  listeners (HtmlVoidEl _ ls) = ls

class HtmlTag a where
  htmlTag :: a -> String

renderAttrsForEl :: forall a. HasAttrs a => a -> String
renderAttrsForEl a =
  let
    attrs'@(Attrs array) = attrs a
  in
    if null array then "" else " " <> render attrs'

-- ** Container element.

data ContainerEl a = ContainerEl ContainerTag Attrs (Array a) (Array Listener)

instance Functor ContainerEl where
  map f (ContainerEl tag as cs ls) = ContainerEl tag as (map f cs) ls

instance HasAttrs (ContainerEl a) where
  attrs (ContainerEl _ as _ _) = as

instance HtmlTag (ContainerEl a) where
  htmlTag (ContainerEl tag _ _ _) = render tag

instance Render a => Render (ContainerEl a) where
  render container =
    "<" <> htmlTag container <> renderAttrsForEl container <> ">"
      <> foldMap render (children container)
      <> "<"
      <> htmlTag container
      <> "/>"

children :: forall a. ContainerEl a -> Array a
children (ContainerEl _ _ xs _) = xs

sequence :: forall m a. Applicative m => ContainerEl (m a) -> m (ContainerEl a)
sequence (ContainerEl tag as cs ls) =
  Traversable.sequence cs <#> \cs' -> ContainerEl tag as cs' ls

data ContainerTag = Div

instance Render ContainerTag where
  render Div = "div"

-- ** Void element.

data HtmlVoidEl = Img

instance HtmlTag HtmlVoidEl where
  htmlTag Img = "img"

instance Render HtmlVoidEl where
  render = htmlTag
