module Pasta.Component where

import Prelude (Unit, ($), (<<<), pure, unit)

import Data.Exists (Exists, mkExists)
import Data.Function (flip)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)

import Pasta.Attribute (DivAttr)
import Pasta.Element (ContainerEl(..), ContainerTag(..), HtmlEl(..))

-- * Component.

type Key = String

type SetState s = s -> Effect Unit

-- | A function from state to node, and some rendering options.
data Component s = Component
  { node :: s -> SetState s -> Node s
  , options :: Options s
  }

-- | Rendering options.
type Options s =
  { hash :: s -> Int
  -- | Key to enable hashing of the component for a given state 's'.
  , key :: Maybe Key
  -- | A function to execute when this component's state is updated.
  , onUpdate :: SetState s
  }

-- | Default options.
options :: forall s. Hashable s => Options s
options = { hash: hash, key: Nothing, onUpdate: \_ -> pure unit }

-- | Construct a component with given 'Options'.
component :: forall s. Options s -> (s -> SetState s -> Node s) -> Component s
component options' node = Component { node, options: options' }

-- | Construct a component with default 'Options' and given 'Key'.
componentK
  :: forall s. Hashable s => Key -> (s -> SetState s -> Node s) -> Component s
componentK key node = Component { node, options: options { key = Just key } }

-- * Child component.

-- | Like 'ChildComponentF' but with type parameter 't' hidden.
type ChildComponent s = Exists (ChildComponentF s)

-- | A component with additional functions to convert parent state 's' to child
-- | state 't', and to update parent state 's' when given child state 't'.
data ChildComponentF s t = ChildComponent
  ((s -> t) /\ (t -> s -> s) /\ Component t)

childComponent
  :: forall s t. (s -> t) /\ (t -> s -> s) /\ Component t -> ChildComponent s
childComponent = mkExists <<< ChildComponent

-- * Node.

-- | A node is either HTML or a child component, think of it like JSX.
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
div as cs = NodeHtmlEl $ HtmlContainerEl $ ContainerEl Div as cs []

div_ :: forall s. Array (Node s) -> Node s
div_ = div []
