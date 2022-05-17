module Pasta.Attempts.P1 where

import Prelude (($), (<>), Unit, const, pure, unit)

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Console (log)

-- | A HTML element including its HTML element children.
data El          state =
    ElContainerEl (ContainerEl state)
  | ElVoidEl      (VoidEl      state)

-- | A HTML element that has children.
data ContainerEl state =
    El     (ElConstructor state (tagName :: TagName)) (Array (El state))
  | Div    (ElConstructor state ()                  ) (Array (El state))

-- | A HTML element that can't have children.
data VoidEl      state =
    VoidEl (ElConstructor state (tagName :: TagName))

-- | Construct the information to render a HTML element (without children).
type ElConstructor state r =
    { state    :: state
    , setState :: (state -> state) -> Effect Unit
    , id       :: Id
    }
  -> Maybe
      { attrs    :: Array Attr  -- ^ Global attributes.
      , onRender :: Effect Unit -- ^ Effect to run after rendering the DOM.
      | r
      }

-- | A HTML tag name such as "div" or "span".
newtype TagName = TagName String

-- | The "id" of a HTML element. "foo" in "<div id="foo">".
newtype Id      = Id      String

-- | A global attribute that extends a HTML element.
--     https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes
data Attr =
    Attr     String  (Maybe String) -- ^ A key and maybe a value.
  | Class    String                 -- ^ A HTML class attribute.
  | Disabled Boolean                -- ^ A HTML disabled attribute

-- | To write functions that work with `ContainerEl` and `VoidEl` we need to be
--   able to convert both to `El`. The user should not need to worry about this.
class ToEl el state where
  toEl :: el state -> El state

instance toElContainerEl :: ToEl ContainerEl state where
  toEl containerEl = ElContainerEl containerEl

instance toElVoidEl      :: ToEl VoidEl      state where
  toEl voidEl      = ElVoidEl      voidEl

append :: forall el state. ToEl el state => ContainerEl state -> el state -> ContainerEl state
append (El  c children) child = (El  c (children <> [toEl child]))
append (Div c children) child = (Div c (children <> [toEl child]))

infixr 1 append as !

-- extend :: forall el state. ToEl el state => ContainerEl state -> Attr -> ContainerEl state
-- extend c _ = c
-- extend (Div c children) attr = (Div (\s -> c s {}) children)

div :: forall state. ContainerEl state
div = Div (const $ Just { attrs : [], onRender : pure unit }) []

-- | A void element with no attributes or 'onRender' function.
void :: forall state. String -> VoidEl state
void tagName = VoidEl $ const $ Just
  { tagName  : TagName tagName
  , attrs    : []
  , onRender : pure unit
  }

test :: forall state. ContainerEl state
test =
  div
  ! div
  ! void "src"

-- data C s = C { tag :: s -> TagName}

type Attrs           = Map.Map String Attr
type Children  state = Array (Component state)
type SetState  state = (state -> state) -> Effect Unit
data Component state = Component
  { render   :: state -> SetState state -> Effect (Attrs /\ TagName)
  , children :: Children state
  }

type DomNodeId          = String
type VDomChildren state = Array (VDom state)
data VDom state         = VDom
  { domNodeId :: DomNodeId
  , render    :: state -> SetState state -> Effect (Attrs /\ TagName)
  , rendered  :: (Attrs /\ TagName)
  , children  :: VDomChildren state
  }

runComponent :: forall state. DomNodeId -> Component state -> state -> Effect Unit
runComponent rootId comp state = pure unit
  -- get the root node
  -- construct a set state function
  -- create a virtual dom

main :: Effect Unit
main = do
  log "🍝"
