module Pasta.Strategy where

import Data.Either (Either)

import Pasta.Render.Raw (Raw)
import Pasta.Run.Class (RunComponent)

-- | Updates to the DOM.
data DomUpdates
  -- | Set inner HTML.
  = InnerHtml Raw String

type Error = String

-- | A strategy of generating DOM updates for a component.
type Strategy o c =
  { instructions :: c -> o -> Either Error DomUpdates
  , run :: forall s. RunComponent o c s
  }
