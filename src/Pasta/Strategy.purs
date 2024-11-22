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
data Strategy o c = Strategy
  { getCache :: Effect c
  , instructions :: c -> o -> Either Error DomUpdates
  , run :: forall s. RunComponent o c s
  , setCache :: c -> Effect Unit
  }
