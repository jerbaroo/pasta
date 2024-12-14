module Pasta.Run.Class where

import Data.Tuple.Nested (type (/\))

import Pasta.Component (Component, UpdateState)

class Run i o v s where
  run :: String -> v -> i -> s -> UpdateState s -> o /\ v

type RunComponent o v s =
  String -> v -> Component s -> s -> UpdateState s -> o /\ v
