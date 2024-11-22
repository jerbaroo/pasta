module Pasta.Run.Class where

import Data.Tuple.Nested (type (/\))

import Pasta.Component (Component, SetState)

class Run o i c s where
  run :: c -> i -> s -> SetState s -> o /\ c

type RunComponent o c s = c -> Component s -> s -> SetState s -> o /\ c
