module Main where

import Prelude hiding (div)

import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (fold)
import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ renderComponent parent { foo: 1, bar: 2 }

type AppState = { foo :: Int, bar :: Int }

parent :: Component AppState
parent _ = div [c _.foo x, c _.bar x]

x :: Component Int
x _ = div [c identity y]

y :: Component Int
y _ = div []

-- * Components.

-- | A function from state 's' to a node.
type Component s = s -> Node s

-- | Like 'ChildComponentF' but with type parameter 't' hidden.
type ChildComponent s = Exists (ChildComponentF s)

-- | A component with additional function from 's' to 't'.
data ChildComponentF s t = ChildComponent ((s -> t) /\ Component t)

childComponent :: forall s t. (s -> t) /\ Component t -> ChildComponent s
childComponent = mkExists <<< ChildComponent

-- * Nodes.

-- | A node can be rendered to HTML.
data Node s
  = NodeChildComponent (ChildComponent s)
  | NodeHtml (Html (Node s))

-- | Lift a child component into a 'Node'.
child :: forall s t. (s -> t) -> Component t -> Node s
child = curry $ NodeChildComponent <<< childComponent

-- | Shorthand for 'child'.
c :: forall s t. (s -> t) -> Component t -> Node s
c = child

-- * HTML.

data Html a
  = HtmlContainer (HtmlContainer a)
  | HtmlInner       String
  | HtmlVoid      HtmlVoid

-- TODO do we need this.
instance Functor Html where
  map f (HtmlContainer container) = HtmlContainer $ map f container
  map _ (HtmlInner string)        = HtmlInner string
  map _ (HtmlVoid void)           = HtmlVoid void

class HtmlTag a where
  htmlTag :: a -> String

-- ** HTML container elements.

data HtmlContainer a
  = Div (Array DivAttr) (Array a)

instance Functor HtmlContainer where
  map f (Div attrs as) = Div attrs $ map f as

instance HtmlTag (HtmlContainer a) where
  htmlTag (Div _ _) = "div"

instance Show a => Show (HtmlContainer a) where
  show container =
       "<" <> htmlTag container <> ">"
    <> fold (map show $ containerChildren container)
    <> "<" <> htmlTag container <> "/>"

div :: forall s. Array (Node s) -> Node s
div = NodeHtml <<< HtmlContainer <<< Div []

containerChildren :: forall a. HtmlContainer a -> Array a
containerChildren (Div _ as) = as

-- ** HTML void elements.

data HtmlVoid
  = Img

instance HtmlTag HtmlVoid where
  htmlTag Img = "img"

instance Show HtmlVoid where
  show = htmlTag

-- ** HTML attributes.

newtype Class = Class String

data DivAttr = DivClass Class

-- * Rendering to HTML.

data RawHtml
  = RawHtmlContainer (HtmlContainer RawHtml)
  | RawHtmlInner     String
  | RawHtmlVoid      HtmlVoid

instance Show RawHtml where
  show (RawHtmlContainer container) = show container
  show (RawHtmlInner string) = string
  show (RawHtmlVoid void) = show void

renderChild :: forall s. ChildComponent s -> s -> RawHtml
renderChild child' s = runExists renderChild' child'
  where
    renderChild' :: forall t. ChildComponentF s t -> RawHtml
    renderChild' (ChildComponent (sToT /\ componentT)) =
      renderComponent componentT $ sToT s

renderComponent :: forall s. Component s -> s -> RawHtml
renderComponent component s = renderNode (component s) s

renderHtml :: forall s. Html (Node s) -> s -> RawHtml
renderHtml (HtmlContainer container) s =
  RawHtmlContainer $ map (flip renderNode s) container
renderHtml (HtmlInner inner) _ = RawHtmlInner inner
renderHtml (HtmlVoid void) _ = RawHtmlVoid void

renderNode :: forall s. Node s -> s -> RawHtml
renderNode (NodeChildComponent child') s = renderChild child' s
renderNode (NodeHtml html) s = renderHtml html s
