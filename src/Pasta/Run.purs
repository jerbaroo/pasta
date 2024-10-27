module Pasta.Run where

import Prelude (class Show, Unit, show, ($), (<<<))

import Effect (Effect)
import Effect.Console (log)

import Pasta.Component (Component)
import Pasta.Render.Class (render)
import Pasta.Render.RawHtmlEl (toRawHtmlEl)

foreign import attach :: String -> String -> Effect Unit

run :: forall s. Show s => String -> Component s -> s -> Effect Unit
run attachId component state = do
  attach attachId $ render $ toRawHtmlEl component state $ log <<< show
