module Pasta.Run where

import Prelude (class Show, Unit, discard, show, ($))

import Effect (Effect)
import Effect.Console (log)

import Pasta.Component (Component(..))
import Pasta.Render.Class (render)
import Pasta.Render.RawHtmlEl (toRawHtmlEl)

foreign import attach :: String -> String -> Effect Unit

run :: forall s. String -> Component s -> s -> Effect Unit
run attachId component s0 = do
  let (Component c) = component
  let
    onUpdate sn = do
      -- Execute the component's 'onUpdate' function.
      c.options.onUpdate sn
      -- Render the given component..
      let html = render $ toRawHtmlEl component sn onUpdate
      -- ..and attach to the DOM.
      attach attachId html
  -- Render the component with initial state s0.
  onUpdate s0
