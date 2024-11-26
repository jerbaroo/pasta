module Pasta.Run.Class where

import Data.Tuple.Nested (type (/\))
import Effect (Effect)

import Pasta.Component (Component, UpdateState)

class Run i o v s where
  run :: v -> i -> s -> UpdateState s -> Effect (o /\ v)

type RunComponent o v s = v -> Component s -> s -> UpdateState s -> Effect (o /\ v)
