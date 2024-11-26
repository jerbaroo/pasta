``` bash
watchexec -e purs,js,yaml -- spago test
```

## Updating State

Consider a component `c :: Component Int` that is being rendered for some state
`s :: Int` and imagine the component also receives as input a callback for
setting state `setState:: Int -> Effect Unit`. To make things more concrete
imagine that the state of the component when being rendered is `5 :: Int`.

Now what would happen if `setState` is called in response to some user input,
say `onclick`, and as first argument to `setState` we pass `s + 1`, in other
words we want the state to increment on user click? The state would be updated
to `6`, and the component tree would re-render. So far so good. Here's the code:

``` purs
\s setState -> div
  [ onClick \_ -> setState $ s + 1 ]
  [ text $ show s ]
```

Now what would happen if the user clicked twice without time to re-render in
between the first and second clicks? State would be updated to the same value
`6` twice in short succession. To avoid this the library could ensure that
callbacks resulting from a previous state/render are disgarded, but then the app
would be temporarily unresponsive between the time of the first callback and the
second render.

Thankfully this is not the case. The intention is to highlight why a callback of
type `s -> Effect Unit` is not a great choice.

Instead in order to update state, a component receives as input a callback for
updating state `updateState :: (s -> Maybe s) -> Effect Unit`. This requires the
component being rendered to pass a function as first argument to `updateState`,
and this function always receives the most recent state as input. These update
functions are executed ATOMICALLY across all components, which means that the
function a component passes to `updateState` as first argument, always receives
a derivative of the most recent GLOBAL state.

To continue with our example above where state should be incremented on user
click, the component would now pass `Just <<< (+ 1)` as first argument to
`updateState`. On first click the state is updated to `6`, and then on second
click the state is updated to `7` even though the component hasn't re-rendered
yet! This is ok, at least the app wasn't unresponsive!

And if you really don't want to respond to the second click IF the component
hasn't rendered inbetween, you can do that:

``` purs
\s updateState -> div
  [ onClick \_ -> updateState \s' -> 
      if s == s' then Just $ s' + 1 else Nothing
  ]
  [ text "hello" ]
```

## Chronology of an Update
