{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE RecursiveDo #-}
module Page.Replay where

import Reflex

import Data.Dom

import Page.Replay.Types
import Page.Replay.Download
import Js.Utils

import Component.Grid
import Component.FileUpload
import Component.FileDownload

import Prelude hiding ((#), (!), (!!))

import Js.Imports
import Js.Types
import qualified Js.FFI as FFI

import Data.Default
import Data.Default.Orphans

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Types (width, height)

replay :: Widget t m => m ()
replay = elClass "div" "replay" $ do
  replayEvent <- download

  widgetHold_ blank $ replayEvent <&> (display . constDyn . initialMap)
  blank

download :: Widget t m => m (Event t Replay)
download = downloadReplay ReplayLocation
  { replay_id = "HOVnMO6cL"
  , server = Server_Main
  }


initialMap :: Replay -> Generals.Map
initialMap Replay{..} = Generals.Map
  { dimensions
  , cells = mountainsMap <> citiesMap <> clearMap
  }
  where
    toCoord index =
      let (j, i) = index `divMod` (dimensions ^. width)
      in (i, j)

    mountainsMap = fromList $
      [ (toCoord x, Mountain)
      | x <- mountains
      ]
    citiesMap = fromList $
      [ (toCoord index, City (Army Neutral size))
      | (index, size) <- zip cities cityArmies
      ]
    clearMap = fromList $
      [((i-1, j-1), def)
      | i <- [1..dimensions ^. width]
      , j <- [1..dimensions ^. height]
      ]
