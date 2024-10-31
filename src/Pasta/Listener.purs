module Pasta.Listener where

import Prelude hiding (div)

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)

import Pasta.Render.Class (class Render, render)

-- * Generic form.

-- | Generic form of a HTML event listener.
newtype Listener = Listener { key :: String, value :: Effect Unit }

-- | Set of types that have event listeners.
class HasListeners a where
  listeners :: a -> Array Listener

-- * Event listeners.

-- | HTML "click" event listner.
onClick :: Effect Unit -> Listener
onClick value = Listener { key: "onclick", value }
