module Pasta.Element where

import Prelude (class Functor, map, ($), (<>))

import Data.Array (null)
import Data.Foldable (foldMap)

import Pasta.Attribute (class HasAttrs, Attrs(..), DivAttr, attrs, toGenericAttr)
import Pasta.Render.Class (class Render, render)

-- * HTML element.

data HtmlEl a
  = HtmlContainerEl (HtmlContainerEl a)
  | HtmlInner       String
  | HtmlVoidEl      HtmlVoidEl

class HtmlTag a where
  htmlTag :: a -> String

renderAttrsForEl :: forall a. HasAttrs a => a -> String
renderAttrsForEl a =
  if null (attrs a) then "" else " " <> render (Attrs $ attrs a)

-- ** Container element.

data HtmlContainerEl a
  = Div (Array DivAttr) (Array a)

instance Functor HtmlContainerEl where
  map f (Div attrs as) = Div attrs $ map f as

instance HasAttrs (HtmlContainerEl a) where
  attrs (Div divAttrs _) = map toGenericAttr divAttrs

instance HtmlTag (HtmlContainerEl a) where
  htmlTag (Div _ _) = "div"

instance Render a => Render (HtmlContainerEl a) where
  render container =
       "<" <> htmlTag container <> renderAttrsForEl container <> ">"
    <> foldMap render (children container)
    <> "<" <> htmlTag container <> "/>"

children :: forall a. HtmlContainerEl a -> Array a
children (Div _ as) = as

-- ** Void element.

data HtmlVoidEl
  = Img

instance HtmlTag HtmlVoidEl where
  htmlTag Img = "img"

instance Render HtmlVoidEl where
  render = htmlTag
