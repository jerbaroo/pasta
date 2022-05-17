module Pasta.Attempts.P2 where

import Prelude (Unit, pure, unit, ($))

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Pasta.Attr
import Pasta.El

-- In order to render we need initial state S.
-- A function from S to El so we can render S.
-- A function from S to El so we can render S.

--------------------------------------------------------------------------------
-- Tags ------------------------------------------------------------------------

data Tag          = TagContainerTag ContainerTag | TagVoidTag VoidTag
data ContainerTag = TagButton Button | TagDiv Div
data VoidTag      = TagImg Img

--------------------------------------------------------------------------------
-- Elements --------------------------------------------------------------------

-- | A function from 'state' to a 'tag' to be rendered.
type Render state tag =
    { state     :: state
    , prevState :: Maybe state
    , setState  :: (state -> state) -> Effect Unit
    , id        :: Id
    }
  -> Maybe
    { tag      :: tag
    , onRender :: Effect Unit
    }

newtype ElBase state tag r = ElBase
  { render :: Render state tag
  | r
  }

type    Children    state = ( children :: Array (El state) )

data    El          state = ElContainerEl (ContainerEl state) | ElVoidEl (VoidEl state)
newtype ContainerEl state = ContainerEl   (ElBase state ContainerTag (Children state))
newtype VoidEl      state = VoidEl        (ElBase state VoidTag      (              ))

-- append :: forall childState state. ContainerEl state -> El childState -> ContainerEl state
-- append container _ = do
--   let (ContainerEl (El)) = container
--   container

example :: Render String Button
example {} =
  Just { tag : Button [ ButtonClass $ Class "Hello" ] [], onRender : pure unit }

-- Button :: forall state. Button state
-- Button =

-- data Cont state = Butt (Array ButtonAttr) (state -> )

-- | Necessary information to render a HTML element (including children).
-- type ContainerElBase state =
--   { render   :: Render state
--   , children :: Array (El state)
--   }

-- | The type of attributes for a specific element.
-- class HasAttrBase el attr where
--   hasAttrBase :: forall state. el state -> (ElBase state attr)

-- instance hasAttrBaseButton :: HasAttrBase Button ButtonAttr where
--   hasAttrBase (Button elBase) = elBase
-- instance hasAttrBaseDiv    :: HasAttrBase Div    DivAttr    where
--   hasAttrBase (Div    elBase) = elBase

-- We need a function that takes some attribute and some element that only
-- compiles if the element accepts that attribute e.g. div and class. Currently
-- div and class are data constructors, but they need their own type if we are
-- going to type-check them against each other.

-- The reason we don't store attributes as '{ onRender :: Effect Unit | r }'
-- where the attributes specific to some HTML element type are in 'r' is so we
-- don't have a huge record of attributes that needs to be evaluated on every
-- render for every element.
