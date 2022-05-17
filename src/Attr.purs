module Pasta.Attr where

import Prelude
import Data.Maybe (Maybe)

newtype Key = Key String

data Value = ValueBoolean Boolean | ValueNumber Number | ValueString String

data Attr = Attr { key :: Key, value :: Value }

class ToAttr a where
  toAttr :: a -> Attr

-- * HTML attributes.

newtype Class = Class String

newtype Disabled = Disabled Boolean

newtype Id = Id String

newtype Src = Src String

-- * HTML attributes permitted per-element.

data ButtonAttr =
    ButtonClass Class
  | ButtonDisabled Disabled

instance ToElAttr Class ButtonAttr where toElAttr = ButtonClass

instance ToElAttr Disabled ButtonAttr where toElAttr = ButtonDisabled

data DivAttr = DivClass Class

instance ToElAttr Class DivAttr where toElAttr = DivClass

data ImgAttr = ImgSrc Src

instance ToElAttr Src ImgAttr where toElAttr = ImgSrc

-- * Convert a HTML attribute to a "per-element" HTML attribute.

class ToElAttr attr elAttr where
  toElAttr :: attr -> elAttr

-- TODO: append to element not to Array
appendAttr
  :: forall attr elAttr
  .  ToElAttr attr elAttr
  => Array elAttr
  -> attr
  -> Array elAttr
appendAttr e a = e <> [ toElAttr a ]
infixr 1 appendAttr as !
