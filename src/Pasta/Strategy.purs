module Pasta.Strategy where

import Prelude (Unit)

import Data.Either (Either)
import Effect (Effect)

import Pasta.Render.Raw (Raw)
import Pasta.Run.Class (RunComponent)

-- | Updates to the DOM.
data DomUpdates
  -- | Set inner HTML.
  = InnerHtml Raw String

type Error = String

-- | A strategy of generating DOM updates for a component.
type Strategy o v =
  { instructions :: v -> v -> o -> Either Error DomUpdates
  , onError :: String -> Effect Unit
  , run :: forall s. RunComponent o v s
  }
