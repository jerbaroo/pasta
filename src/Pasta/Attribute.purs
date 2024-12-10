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

class GetAttrs a where
  getAttrs :: a -> Array Attr

class SetAttrs a where
  setAttrs :: Array Attr -> a -> a

newtype RenderAttrs = RenderAttrs (Array Attr)

instance Render RenderAttrs where
  render (RenderAttrs attrs) = intercalate " " $ map render attrs

-- * Attributes.

newtype Class = Class String

instance ToAttr Class where
  toAttr (Class class') = Attr
    { key: "class", value: Just class' }

class ClassAttr a where
  class' :: String -> a

-- * Per-element attributes.

data DivAttr = DivClass Class

instance ClassAttr DivAttr where
  class' = DivClass <<< Class

instance ToAttr DivAttr where
  toAttr (DivClass class') = toAttr class'
