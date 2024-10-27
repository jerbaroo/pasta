module Pasta.Component where

import Prelude (Unit, ($), (<<<))

import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)

import Pasta.Attribute (DivAttr)
import Pasta.Element (HtmlContainerEl(..), HtmlEl(..))

-- * Component.

type SetState s = s -> Effect Unit

-- | A function from state 's' to renderable node. Hashable for performance.
data Component s = Component
  -- | Set this string to hash the rendered result for a given state 's'.
  { key :: Maybe String
  -- | A function from state 's' and state update function to renderable node.
  , node :: s -> SetState s -> Node s
  }

component :: forall s. String -> (s -> SetState s -> Node s) -> Component s
component key node = Component { key: Just key, node }

component_ :: forall s. (s -> SetState s -> Node s) -> Component s
component_ node = Component { key: Nothing, node }

-- * Child component.

-- | Like 'ChildComponentF' but with type parameter 't' hidden.
type ChildComponent s = Exists (ChildComponentF s)

-- | A component with additional functions to convert parent state 's' to child
-- | state 't', and to update parent state 's' when given child state 't'.
data ChildComponentF s t = ChildComponent ((s -> t) /\ (t -> s -> s) /\ Component t)

childComponent :: forall s t. (s -> t) /\ (t -> s -> s) /\ Component t -> ChildComponent s
childComponent = mkExists <<< ChildComponent

-- * Node.

-- | A node can be rendered to HTML.
data Node s
  = NodeChildComponent (ChildComponent s)
  | NodeHtmlEl (HtmlEl (Node s))

-- | Lift a child component into a 'Node'.
child :: forall s t. (s -> t) -> (t -> s -> s) -> Component t -> Node s
child a b c' = NodeChildComponent $ childComponent $ a /\ b /\ c'

-- | Shorthand for 'child'.
c :: forall s t. (s -> t) -> (t -> s -> s) -> Component t -> Node s
c = child

-- * HTML nodes.

text :: forall s. String -> Node s
text = NodeHtmlEl <<< HtmlInner

div :: forall s. Array DivAttr -> Array (Node s) -> Node s
div attrs = NodeHtmlEl <<< HtmlContainerEl <<< Div attrs

div_ :: forall s. Array (Node s) -> Node s
div_ = NodeHtmlEl <<< HtmlContainerEl <<< Div []
