module Pasta.Event where

import Prelude hiding (div)

-- | Class of types that are events.
class IsEvent a where
  name :: String

data ClickEvent

instance IsEvent ClickEvent where
  name = "click"
