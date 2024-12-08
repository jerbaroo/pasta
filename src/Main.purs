module Main where

import Prelude (Unit, show, ($), (<>))

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Pasta.Attribute (class')
import Pasta.Cook as Pasta
import Pasta.Component (Component, c, component, options)
import Pasta.Node
import Pasta.Strategy.Flat (innerHtml, emptyVDom)

main :: Effect Unit
main = Pasta.cook "my-app" (innerHtml "root") emptyVDom parent { foo: 1, bar: 2 }

type AppState = { foo :: Int, bar :: Int }

parent :: Component AppState
parent = component
  options { key = Just "parent", onUpdate = \s -> log $ "parent: " <> show s }
  \_ _ ->
    div_
      [ class' "hello" ]
      [ c _.foo (\t s -> s { foo = t }) child
      , c _.bar (\t s -> s { bar = t }) child
      ]

child :: Component Int
child = component
  options { key = Just "child", onUpdate = \s -> log $ "child: " <> show s }
  \s _ -> div__ [ text $ show s ]
