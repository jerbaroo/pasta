module Pasta.Listener where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Data.Exists (Exists, mkExists, runExists)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

import Pasta.Attribute (Attr(..))
import Pasta.Event (class IsEvent)
import Pasta.Event as Event
import Unsafe.Coerce (unsafeCoerce)

type Handler = EffectFn1 (forall e. e) Unit

handler :: Listener -> Handler
handler = runExists \(Listener l) -> unsafeCoerce $ mkEffectFn1 l.f

data ListenerF e = Listener { f :: e -> Effect Unit, on :: String }

type Listener = Exists ListenerF

eventName :: Listener -> String
eventName = runExists \(Listener l) -> l.on

class GetListeners e1 l where
  getListeners :: e1 -> Array l

class SetListeners l e1 e2 where
  setListeners :: Array l -> e1 -> e2

listenerToAttr :: String -> Listener -> Attr
listenerToAttr value = runExists \(Listener l) ->
  Attr { key: "on" <> l.on, value: Just value }

on :: forall e. IsEvent e => (e -> Effect Unit) -> Listener
on f = mkExists $ Listener { f, on: Event.name @e }

onClick :: (Event.ClickEvent -> Effect Unit) -> Listener
onClick = on
