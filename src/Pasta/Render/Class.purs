module Pasta.Render.Class where

-- import Prelude

-- import Data.Map (Map)
-- import Effect (Effect)

-- type Functions = Map String (Effect Unit)
-- type Rendered = { functions :: Functions, html :: String }

-- | Set of types that can be rendered to HTML.
class Render a where
  render :: a -> String
