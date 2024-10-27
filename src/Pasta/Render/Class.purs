module Pasta.Render.Class where

-- | Set of types that can be rendered to HTML strings.
class Render a where
  render :: a -> String
