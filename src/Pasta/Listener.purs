module Pasta.Listener where

import Prelude hiding (div)

import Data.Exists (Exists, mkExists)
import Effect (Effect)

import Pasta.Event (class IsEvent, name)

data ListenerF e = Listener { f :: e -> Effect Unit, on :: String }

type Listener = Exists ListenerF

on :: forall e. IsEvent e => (e -> Effect Unit) -> Listener
on f = mkExists $ Listener { f, on: name @e }

class HasListeners a where
  listeners :: a -> Array Listener
