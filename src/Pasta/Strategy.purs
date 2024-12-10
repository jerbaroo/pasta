module Pasta.Strategy where

import Prelude (Unit)

import Data.Either (Either)
import Effect (Effect)

import Pasta.Listener (Handler)
import Pasta.Render.Raw (Raw)
import Pasta.Run.Class (RunComponent)

-- | Updates to the DOM.
data DomUpdates
  -- | Set inner HTML.
  = InnerHtml Raw String
  -- | Register a global function.
  | RegisterFunc String Handler

-- | A strategy of generating DOM updates for a component.
type Strategy o v =
  { instructions :: v -> v -> o -> Either String (Array DomUpdates)
  , onError :: String -> Effect Unit
  , run :: forall s. RunComponent o v s
  }
