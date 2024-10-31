module Pasta.Attribute where

import Prelude hiding (div)

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))

import Pasta.Render.Class (class Render, render)

-- * Generic form.

-- | Generic form of a HTML attribute.
data GenericAttr = GenericAttr { key :: String, value :: Maybe String }

instance Render GenericAttr where
  render (GenericAttr { key, value }) =
    case value of
      Nothing -> key
      Just value' -> key <> "='" <> value' <> "'"

-- | Set of types that are attributes (convertible to 'GenericAttr').
class ToGenericAttr a where
  toGenericAttr :: a -> GenericAttr

-- | Set of types that have attributes.
class HasAttrs a where
  attrs :: a -> Array GenericAttr

newtype Attrs = Attrs (Array GenericAttr)

instance Render Attrs where
  render (Attrs attrs) = intercalate " " $ map render attrs

-- * Attributes.

-- | HTML class attribute.
newtype Class = Class String

instance ToGenericAttr Class where
  toGenericAttr (Class class') = GenericAttr { key: "class", value: Just class' }

class ClassAttr a where
  class' :: String -> a

-- * Per-element attributes.

-- | HTML div attributes.
data DivAttr = DivClass Class

instance ClassAttr DivAttr where
  class' = DivClass <<< Class

instance ToGenericAttr DivAttr where
  toGenericAttr (DivClass class') = toGenericAttr class'
