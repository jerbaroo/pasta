module Pasta.El where

import Data.Maybe (Maybe(Just))
import Prelude
import Pasta.Attr
import Pasta.Tag

data El = El
  { attrs    :: Array Attr
  , children :: Maybe (Array El)
  , tag      :: Tag
  }

class ToEl a where
  toEl :: a -> El

-- * Individual HTML elements.

data Button = Button (Array ButtonAttr) (Array El)

instance HasTag Button where tag (Button _ _) = Tag "button"

instance ToEl Button where
  toEl (Button attrs children) =
    El { attrs, children: Just children }

-- | Construct a @Button@ with no attributes.
-- TODO: should have default attributes.
button :: Button
button  = Button [] []

data Div = Div (Array DivAttr)

instance HasTag Div where tag (Div _) = Tag "div"

newtype Img = Img (Array ImgAttr)

instance HasTag Img where tag (Img _) = Tag "img"
