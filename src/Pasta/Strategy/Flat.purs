module Pasta.Strategy.Flat where

import Prelude (map, show, ($), (<#>), (<<<), (<>))

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Console (log)

import Pasta.Element (Element(..), sequence)
import Pasta.Strategy (DomUpdates(..), Strategy)
import Pasta.Render.Raw (Raw(..))
import Pasta.Run.Flat (ComponentRef, Flat(..), VDom(..), runComponent)

emptyVDom :: VDom
emptyVDom = VDom
  { components: Map.empty
  , listeners: Map.empty
  , nextListenerId: 0
  }

-- | Convert 'Flat' HTML to 'Raw' HTML via provided virtual DOM.
flatToRaw :: VDom -> Flat -> Either ComponentRef Raw
flatToRaw vDom@(VDom v) (FlatComponentRef ref) =
  case Map.lookup ref v.components of
    Nothing -> Left ref
    Just flat -> flatToRaw vDom flat
flatToRaw vDom (FlatElement (ElementContainer container)) =
  sequence (map (flatToRaw vDom) container) <#> Raw <<< ElementContainer
flatToRaw _ (FlatElement (ElementInner inner)) = Right $ Raw $ ElementInner inner
flatToRaw _ (FlatElement (ElementVoid void)) = Right $ Raw $ ElementVoid void

-- | A simple strategy that re-renders the entire app on every state update.
innerHtml :: String -> Strategy Flat VDom
innerHtml attachId =
  { instructions: \_ vDomNew f -> case flatToRaw vDomNew f of
      Left (key /\ hash) -> Left $
        "error: could not find " <> key <> "-" <> show hash <> " in VDom"
      Right raw -> Right $ InnerHtml raw attachId
  , onError: log
  , run: runComponent
  }
