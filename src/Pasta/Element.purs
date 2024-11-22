module Pasta.Element where

import Prelude (class Functor, map, (<>))

import Control.Applicative (class Applicative)
import Data.Foldable (foldMap)
import Data.Functor ((<#>))
import Data.Traversable as Traversable

import Pasta.Attribute (class HasAttrs, Attrs(..), attrs)
import Pasta.Listener (class HasListeners, Listener)
import Pasta.Render.Class (class Render, render)

-- * HTML element.

-- | A HTML element.
data Element a
  = ElementContainer (Container a)
  | ElementInner String
  | ElementVoid Void

instance HasListeners (Element a) where
  listeners (ElementContainer (Container _ _ _ ls)) = ls
  listeners (ElementInner _) = []
  listeners (ElementVoid (Void _ _ ls)) = ls

class ElementTag a where
  elementTag :: a -> String

renderAttrsForEl :: forall a. HasAttrs a => a -> String
renderAttrsForEl a =
  case attrs a of
    Attrs [] -> ""
    attrs' -> " " <> render attrs'

-- * Container element.

data Container a = Container ContainerTag Attrs (Array a) (Array Listener)

instance ElementTag (Container a) where
  elementTag (Container tag _ _ _) = render tag

instance Functor Container where
  map f (Container tag as cs ls) = Container tag as (map f cs) ls

instance HasAttrs (Container a) where
  attrs (Container _ as _ _) = as

instance Render a => Render (Container a) where
  render container =
    "<" <> elementTag container <> renderAttrsForEl container <> ">"
      <> foldMap render (children container)
      <> "<"
      <> elementTag container
      <> "/>"

children :: forall a. Container a -> Array a
children (Container _ _ xs _) = xs

sequence :: forall m a. Applicative m => Container (m a) -> m (Container a)
sequence (Container tag as cs ls) =
  Traversable.sequence cs <#> \cs' -> Container tag as cs' ls

data ContainerTag = Div

instance Render ContainerTag where
  render Div = "div"

-- * Void element.

data Void = Void VoidTag Attrs (Array Listener)

instance ElementTag Void where
  elementTag (Void tag _ _) = render tag

instance HasAttrs Void where
  attrs (Void _ as _) = as

instance Render Void where
  render void = "<" <> elementTag void <> renderAttrsForEl void <> "/>"

data VoidTag = Img

instance Render VoidTag where
  render Img = "img"
