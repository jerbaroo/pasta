module Main where

import Prelude (Unit, show, ($))

import Effect (Effect)
import Pasta.Attribute (class')
import Pasta.Component (Component, c, component, div, div_, text)
import Pasta.Run (run)

main :: Effect Unit
main = run "root" parent { foo: 1, bar: 2 }

type AppState = { foo :: Int, bar :: Int }

parent :: Component AppState
parent = component "counter-example" \_ _ ->
  div
    [ class' "hello" ]
    [ c _.foo (\t s -> s { foo = t }) counter
    , c _.bar (\t s -> s { bar = t }) counter
    ]

counter :: Component Int
counter = component "counter" \s _ -> div_ [ text $ show s ]
