module Main where

import Prelude (Unit, discard, show, ($), (<<<))

import Effect (Effect)
import Effect.Console (log)

import Pasta.Attribute (class')
import Pasta.Component (Component, c, component, div, div_, text)
import Pasta.Render.Class (render)
import Pasta.Render.RawHtmlEl (renderComponent)

main :: Effect Unit
main = do
  log "🍝"
  log $ render $ renderComponent parent { foo: 1, bar: 2 } $ log <<< show

type AppState = { foo :: Int, bar :: Int }

parent :: Component AppState
parent = component "counter-example" \_ _ ->
  div
    [ class' "hello" ]
    [ c _.foo (\t s -> s{foo=t}) counter
    , c _.bar (\t s -> s{bar=t}) counter
    ]

counter :: Component Int
counter = component "counter" \s _ -> div_ [text $ show s]
