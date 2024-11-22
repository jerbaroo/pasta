module Pasta.Attribute where

import Prelude hiding (div)

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))

import Pasta.Render.Class (class Render, render)

-- | A HTML attribute.
newtype Attr = Attr { key :: String, value :: Maybe String }

instance Render Attr where
  render (Attr { key, value }) =
    case value of
      Nothing -> key
      Just value' -> key <> "='" <> value' <> "'"

class ToAttr a where
  toAttr :: a -> Attr

-- | Newtype for rendering array of attribtues.
newtype Attrs = Attrs (Array Attr)

-- | Set of types that have HTML attributes.
class HasAttrs a where
  attrs :: a -> Attrs

instance Render Attrs where
  render (Attrs attrs) = intercalate " " $ map render attrs

toAttrs :: forall a. ToAttr a => Array a -> Attrs
toAttrs = Attrs <<< map toAttr

-- * Attributes.

-- | HTML class attribute.
newtype Class = Class String

instance ToAttr Class where
  toAttr (Class class') = Attr
    { key: "class", value: Just class' }

class ClassAttr a where
  class' :: String -> a

-- * Per-element attributes.

-- | HTML "div" attributes.
data DivAttr = DivClass Class

instance ClassAttr DivAttr where
  class' = DivClass <<< Class

instance ToAttr DivAttr where
  toAttr (DivClass class') = toAttr class'
