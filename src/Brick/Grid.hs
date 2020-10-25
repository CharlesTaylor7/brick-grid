{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Grid
  ( GridStyle(..)
  , drawGrid
  ) where

import Data.Traversable (for)
import Data.List (intercalate, intersperse)
import Control.Monad.Reader (ask)

import Lens.Micro (Lens', (&), (<&>), to)
import Lens.Micro.Mtl (view)

import Brick (Widget, str, vBox, hBox, txt, textWidth)
import Brick.Widgets.Border.Style (BorderStyle)

import Brick.Grid.TH (suffixLenses)

type TileContents = String

data GridStyle = GridStyle
  { borderStyle :: BorderStyle
  , cellWidth :: Int
  , gridWidth :: Int
  , gridHeight :: Int
  , drawTileWith :: (Int, Int) -> TileContents
  }

suffixLenses ''GridStyle
suffixLenses ''BorderStyle


drawGrid :: GridStyle -> Widget name
drawGrid = do
  width  <- view gridWidthL
  height <- view gridHeightL
  let
    rowIndices    = [0 .. width - 1]
    columnIndices = [0 .. height - 1]

  drawTile <- ask drawTileToFit
  rows <-
    for columnIndices $ \y ->
      insertVBorders $
        rowIndices <&> (\x -> str $ drawTile (x, y))

  insertHBorders rows


drawTileToFit :: GridStyle -> (Int, Int) -> TileContents
drawTileToFit = do
  cellWidth <- view cellWidthL
  drawTile <- view drawTileWithL
  let
    fitToCell result =
      case textWidth result of
        n
          -- exact fit
          | n == cellWidth -> result
          -- truncate to fit
          | n > cellWidth ->  take cellWidth result
          -- pad
          -- TODO: center
          | otherwise -> replicate (cellWidth - n) ' ' <> result

  pure $ fitToCell . drawTile


insertVBorders :: [Widget name] -> GridStyle -> Widget name
insertVBorders cells = do
  v <- view $ borderStyleL . bsVerticalL . to (str . pure)
  pure . hBox . (v:) . (<> [v]) . intersperse v $ cells

insertHBorders :: [Widget name] -> GridStyle -> Widget name
insertHBorders cells = do
  h1 <- hBorder Top
  h2 <- hBorder Bottom
  h3 <- hBorder Middle
  pure . vBox . (h1 :) . (<> [h2]) . intersperse h3 $ cells


hBorder :: VLocation -> GridStyle -> Widget name
hBorder v = do
  cellWidth <- view cellWidthL
  mapWidth <- view gridWidthL
  innerBorder <- view $ borderStyleL . borderStyleLens v Center
  startCorner <- view $ borderStyleL . borderStyleLens v Start
  endCorner <- view $ borderStyleL . borderStyleLens v End
  pipe <- view $ borderStyleL . bsHorizontalL
  replicate cellWidth pipe
      & replicate mapWidth
      & intercalate [innerBorder]
      & \row -> [startCorner] <> row <> [endCorner]
      & str
      & pure

data VLocation = Bottom | Middle | Top
data HLocation = Start | Center | End

borderStyleLens :: VLocation -> HLocation -> Lens' BorderStyle Char
borderStyleLens Bottom Start  = bsCornerBLL
borderStyleLens Bottom Center = bsIntersectBL
borderStyleLens Bottom End    = bsCornerBRL
borderStyleLens Middle Start  = bsIntersectLL
borderStyleLens Middle Center = bsIntersectFullL
borderStyleLens Middle End    = bsIntersectRL
borderStyleLens Top Start     = bsCornerTLL
borderStyleLens Top Center    = bsIntersectTL
borderStyleLens Top End       = bsCornerTRL
