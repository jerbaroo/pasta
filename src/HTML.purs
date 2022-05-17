-- | Intermediate representation when rendering. El -> HTML -> String
module Pasta.HTML where

import Data.Either (Either)
import Data.Map (Map)

-- | The key of a HTML attribute.
newtype Key = Key String

-- | The value of a HTML attribute.
--
-- The different constructors determine how the HTML will be rendered:
--
-- (Key "checked") -> ValueBoolean True  -> "checked"
-- (Key "checked") -> ValueBoolean False -> ""
-- (Key "checked") -> ValueNumber  25    -> "checked=25"
-- (Key "checked") -> ValueString  "foo" -> "checked='foo'"
--
-- Note on @ValueBoolean@: "The presence of a boolean attribute on an element
-- represents the true value, and the absence of the attribute represents the
-- false value." - https://html.spec.whatwg.org/#boolean-attribute
data Value = ValueBoolean Boolean | ValueNumber Number | ValueString String

-- | One attribute of a HTML element. A key and maybe a value.
data Attr = Attr Key Value

-- | The attributes of a HTML element. Attributes keys are unique.
newtype Attrs = Attrs (Map Key Value)

-- | The name of a HTML tag e.g. "div".
newtype TagName = TagName String

-- | A HTML element without children.
data VoidEl = VoidEl TagName Attrs

-- | A HTML element that might have children.
data ContainerEl = ContainerEl TagName Attrs (Array El)

-- | A HTML element.
newtype El = El (Either ContainerEl VoidEl)

-- attrToHTML :: Attr -> Maybe (String /\ Maybe String)
-- attrToHTML (Attr      key valueMaybe) = Just $ key     /\ valueMaybe
-- attrToHTML (Class         class_    ) = Just $ "class" /\ Just class_
-- attrToHTML (Disabled      disabled  ) = if disabled then Just ("disabled" /\ Nothing) else Nothing
