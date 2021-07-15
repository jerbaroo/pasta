module Pasta.Take2 where

import Pasta.Types as T

-- import Prelude (($), (<>), Unit, const, pure, unit)
import Prelude

import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)

--------------------------------------------------------------------------------
-- HTML representation ---------------------------------------------------------

data    HTMLAttr        = HTMLAttr              String   (Maybe String)
newtype HTMLAttrs       = HTMLAttrs       (Map  String   (Maybe String))
newtype HTMLTagName     = HTMLTagName     String
data    HTMLVoidEl      = HTMLVoidEl      HTMLTagName HTMLAttrs
data    HTMLContainerEl = HTMLContainerEl HTMLTagName HTMLAttrs (Array HTMLEl)
newtype HTMLEl          = HTMLEl          (Either HTMLContainerEl HTMLVoidEl)

--------------------------------------------------------------------------------
-- Attributes ------------------------------------------------------------------

newtype Class    = Class    String
newtype Disabled = Disabled Boolean
newtype Id       = Id       String
newtype Src      = Src      String

data ButtonAttr  = ButtonClass Class | ButtonDisabled Disabled
data DivAttr     = DivClass    Class
data ImgAttr     = ImgSrc      Src

class ToElAttr attr elAttr where
  toElAttr :: attr -> elAttr

instance toElAttrButtonClass    :: ToElAttr Class    ButtonAttr where toElAttr = ButtonClass
instance toElAttrDivClass       :: ToElAttr Class    DivAttr    where toElAttr = DivClass
instance toElAttrButtonDisabled :: ToElAttr Disabled ButtonAttr where toElAttr = ButtonDisabled
instance toElAttrImgSrc         :: ToElAttr Src      ImgAttr    where toElAttr = ImgSrc

appendAttr :: forall attr elAttr. ToElAttr attr elAttr => Array elAttr -> attr -> Array elAttr
appendAttr e a = e <> [ toElAttr a ]

-- infixr 1 appendAttr as !

--------------------------------------------------------------------------------
-- Tags ------------------------------------------------------------------------

data Tag          = TagContainerTag ContainerTag | TagVoidTag VoidTag
data ContainerTag = TagButton Button | TagDiv Div
data VoidTag      = TagImg Img

newtype Button  = Button (Array ButtonAttr)
button          = Button []
newtype Div     = Div    (Array DivAttr   )
newtype Img     = Img    (Array ImgAttr   )

class HasTagName a where
  tagName :: a -> String

instance hasTagNameButton :: HasTagName Button where tagName (Button _) = "button"
instance hasTagNameDiv    :: HasTagName Div    where tagName (Div    _) = "div"
instance hasTagNameImg    :: HasTagName Img    where tagName (Img    _) = "img"

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

example :: Render String Button
example { state } =
  Just { tag : Button [ ButtonClass $ Class "Hello" ], onRender : pure unit }

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
