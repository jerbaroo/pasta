module Main where

import Prelude (Unit, show, ($), (<<<))

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Pasta.Attribute (class')
import Pasta.Component (Component, c, component, componentK, div, div_, options, text)
import Pasta.Run (run)

main :: Effect Unit
main = run "root" parent { foo: 1, bar: 2 }

type AppState = { foo :: Int, bar :: Int }

parent :: Component AppState
parent = component
  (options { key = Just "counter-example", onUpdate = log <<< show })
  \_ _ ->
    div
      [ class' "hello" ]
      [ c _.foo (\t s -> s { foo = t }) counter
      , c _.bar (\t s -> s { bar = t }) counter
      ]

counter :: Component Int
counter = componentK "counter" \s _ -> div_ [ text $ show s ]
