module Main where

import Prelude (Unit, show, ($), (<>), (<<<))

import Data.Eq (class Eq)
import Data.Hashable (class Hashable, hash)
import Data.Newtype (class Newtype, modify, unwrap)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Effect (Effect)
import Effect.Class.Console (log)

import Pasta.Attribute (class')
import Pasta.Cook (cookDefault) as Pasta
import Pasta.Component (class SSRState, Component, c, component, options)
import Pasta.Component (readStateUnsafe, showStateUnsafe) as Pasta
import Pasta.Listener (onClick)
import Pasta.Node

main :: Effect Unit
main = Pasta.cookDefault "my_app" "root" app Nothing

-- * State definition.

newtype AppState = AppState { foo :: Int, bar :: Int }

derive instance Eq AppState

instance Hashable AppState where
  hash = hash <<< unwrap

derive instance Newtype AppState _

instance Show AppState where
  show = show <<< unwrap

instance SSRState AppState where
  readState = Pasta.readStateUnsafe
  showState = Pasta.showStateUnsafe

-- * App.

app :: Component AppState
app = component
  options { key = Just "parent", onUpdate = \s -> log $ "parent: " <> show s }
  \_ _ ->
    div
      [ class' "hello" ]
      [ onClick \_ -> log "clicked parent" ]
      [ c (_.foo <<< unwrap) (\t -> modify \s -> s { foo = t }) child
      , c (_.bar <<< unwrap) (\t -> modify \s -> s { bar = t }) child
      ]

child :: Component Int
child = component
  options { key = Just "child", onUpdate = \s -> log $ "child: " <> show s }
  \s _ -> div__ [ text $ show s ]
