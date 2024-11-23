module Pasta.Run.Class where

import Data.Tuple.Nested (type (/\))

import Pasta.Component (Component, SetState)

class Run i o v s where
  run :: v -> i -> s -> SetState s -> o /\ v

type RunComponent o v s = v -> Component s -> s -> SetState s -> o /\ v
