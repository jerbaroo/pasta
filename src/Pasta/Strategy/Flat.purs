module Pasta.Strategy.Flat where

import Prelude (map, show, ($), (<#>), (<<<), (<>))

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Pasta.Element (Element(..), sequence)
import Pasta.Strategy (DomUpdates(..), Strategy)
import Pasta.Render.Raw (Raw(..))
import Pasta.Run.Flat (Cache, ComponentRef, Flat(..), runComponent)

-- | Convert 'Flat' HTML to 'Raw' HTML via provided cache.
flatToRaw :: Cache -> Flat -> Either ComponentRef Raw
flatToRaw c (FlatComponentRef ref) =
  case Map.lookup ref c of
    Nothing -> Left ref
    Just flat -> flatToRaw c flat
flatToRaw c (FlatElement (ElementContainer container)) =
  sequence (map (flatToRaw c) container) <#> Raw <<< ElementContainer
flatToRaw _ (FlatElement (ElementInner inner)) = Right $ Raw $ ElementInner inner
flatToRaw _ (FlatElement (ElementVoid void)) = Right $ Raw $ ElementVoid void

-- | A simple strategy that re-renders the entire app on every state update.
innerHtml :: String -> Strategy Flat Cache
innerHtml attachId =
  { instructions: \c f -> case flatToRaw c f of
      Left (key /\ hash) -> Left $
        "error: could not find " <> key <> "-" <> show hash <> " in cache"
      Right raw -> Right $ InnerHtml raw attachId
  , run: runComponent
  }
