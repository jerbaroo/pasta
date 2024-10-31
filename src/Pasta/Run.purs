module Pasta.Run where

import Prelude (class Show, Unit, discard, show, ($))

import Effect (Effect)
import Effect.Console (log)

import Pasta.Component (Component)
import Pasta.Render.Class (render)
import Pasta.Render.RawHtmlEl (toRawHtmlEl)

foreign import attach :: String -> String -> Effect Unit

run :: forall s. Show s => String -> Component s -> s -> Effect Unit
run attachId component s0 = do
  let onUpdate sn = do
        log $ show sn
        run attachId component sn
  attach attachId $ render $ toRawHtmlEl component s0 onUpdate
