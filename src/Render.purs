module Pasta.Render where

import Prelude (($), (<<<), (<>), map)

import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Pasta.Types (Attr(..), TagName(..))

-- | Instead of rendering directly to a 'String' we convert to a nested 'Maybe'
--   because we are less likely to write an incorrect implementation. The nested
--   'Maybe' type has the following semantics:
--     - Nothing                     -> don't render the attribute
--     - Just ("foo" /\ Nothing)     -> render the attribute key only
--     - Just ("foo" /\ Just "bar")  -> render the attribute key and value
attrToNestedMaybe :: Attr -> Maybe (String /\ Maybe String)
attrToNestedMaybe (Attr      key valueMaybe) = Just $ key     /\ valueMaybe
attrToNestedMaybe (Class         class_    ) = Just $ "class" /\ Just class_
attrToNestedMaybe (Disabled      disabled  ) = if disabled then Just ("disabled" /\ Nothing) else Nothing

-- | Once an attribute is converted to a nested 'Maybe' rendering is trivial.
renderNestedMaybe :: Maybe (String /\ Maybe String) -> String
renderNestedMaybe  Nothing                      = ""
renderNestedMaybe (Just   (key /\ Nothing     )) = key
renderNestedMaybe (Just   (key /\ (Just value))) = key <> "=\"" <> value <> "\""

-- | Render an array of attributes as e.g. "class="foo" disabled".
--
-- Currently duplicate entries will be overwritten by the last duplicate.
renderAttrs :: Array Attr -> String
renderAttrs = intercalate " " <<< map (renderNestedMaybe <<< attrToNestedMaybe)

-- | Render a start tag including attributes as e.g. "<div class="foo">".
renderStartTag :: TagName -> Array Attr -> String
renderStartTag (TagName tagName) attrs =
  "<" <> tagName <> " " <> renderAttrs attrs <> ">"
