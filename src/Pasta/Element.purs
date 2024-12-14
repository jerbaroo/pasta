module Pasta.Element where

import Prelude (class Functor, map, show, ($), (+), (<$>), (<>))
import Prelude as P

import Control.Applicative (class Applicative)
import Data.Array ((..), length, zip)
import Data.Foldable (foldMap)
import Data.Functor ((<#>))
import Data.Traversable as Traversable
import Data.Tuple.Nested (type (/\), (/\))

import Pasta.Attribute (class GetAttrs, class SetAttrs, Attr, RenderAttrs(..), getAttrs, setAttrs)
import Pasta.Listener (class GetListeners, class SetListeners, Handler, Listener, getListeners, handler, listenerToAttr, setListeners)
import Pasta.Render.Class (class Render, render)

-- * HTML element.

-- | A HTML element with children of type 'a' and event listeners of type 'l'.
data Element l a
  = ElementContainer (Container l a)
  | ElementInner String
  | ElementVoid (Void l)

class ElementTag a where
  elementTag :: a -> String

renderAttrsForEl :: forall a. GetAttrs a => a -> String
renderAttrsForEl a =
  case getAttrs a of
    []    -> ""
    attrs -> " " <> render (RenderAttrs attrs)

listenersToAttrs :: forall e1 e2.
  GetListeners e1 Listener => SetListeners P.Void e1 e2 => SetAttrs e2 =>
  String -> Int -> e1 -> (Int /\ Array (String /\ Handler) /\ e2)
listenersToAttrs prefix funcId e =
  let ls        = getListeners e
      maxFuncId = funcId + length ls
      names     = funcId .. maxFuncId <#> \i -> "__" <> prefix <> "_" <> show i
      attrs     = zip names ls <#> \(n /\ l) -> listenerToAttr (n <> "()") l
  in ((maxFuncId + 1) /\ (zip names (handler <$> ls)) /\ (setAttrs attrs $ setListeners @P.Void [] e))

-- * Container element.

data Container l a = Container ContainerTag (Array Attr) (Array a) (Array l)

instance ElementTag (Container l a) where
  elementTag (Container tag _ _ _) = render tag

instance Functor (Container l) where
  map f (Container tag as cs ls) = Container tag as (map f cs) ls

instance GetAttrs (Container l a) where
  getAttrs (Container _ as _ _) = as

instance GetListeners (Container l a) l where
  getListeners (Container _ _ _ ls) = ls

instance Render a => Render (Container l a) where
  render container =
    "<" <> elementTag container <> renderAttrsForEl container <> ">"
      <> foldMap render (children container)
      <> "<"
      <> elementTag container
      <> "/>"

instance SetAttrs (Container l a) where
  setAttrs as' (Container tag as cs ls) = (Container tag (as <> as') cs ls)

instance SetListeners l (Container x a) (Container l a) where
  setListeners ls (Container tag as cs _) = Container tag as cs ls

children :: forall a l. Container l a -> Array a
children (Container _ _ xs _) = xs

sequence :: forall m a l.
  Applicative m => Container l (m a) -> m (Container l a)
sequence (Container tag as cs ls) =
  Traversable.sequence cs <#> \cs' -> Container tag as cs' ls

data ContainerTag = Div

instance Render ContainerTag where
  render Div = "div"

-- * Void element.

data Void l = Void VoidTag (Array Attr) (Array l)

instance ElementTag (Void l) where
  elementTag (Void tag _ _) = render tag

instance GetAttrs (Void l) where
  getAttrs (Void _ as _) = as

instance GetListeners (Void l) l where
  getListeners (Void _ _ ls) = ls

instance Render (Void l) where
  render void = "<" <> elementTag void <> renderAttrsForEl void <> "/>"

instance SetAttrs (Void l) where
  setAttrs as' (Void tag as ls) = (Void tag (as <> as') ls)

instance SetListeners l (Void x) (Void l) where
  setListeners ls (Void tag as _) = Void tag as ls

data VoidTag = Img

instance Render VoidTag where
  render Img = "img"
