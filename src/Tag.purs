module Pasta.Tag where

newtype Tag = Tag String

class HasTag a where
  tag :: a -> Tag
