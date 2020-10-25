{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Grid
  ( GridStyle(..)
  , drawGrid
  ) where

import Data.Traversable (for)
import Data.List (intercalate, intersperse)

import Lens.Micro (Lens', (&), to)
import Lens.Micro.Mtl (view)

import Brick (Widget, str, vBox, hBox)
import Brick.Widgets.Border.Style (BorderStyle)

import Brick.Grid.TH (suffixLenses)

type Tile = (Int, Int)

data GridStyle = GridStyle
  { borderStyle :: BorderStyle
  , cellSize :: Int
  , gridWidth :: Int
  , gridHeight :: Int
  , drawTile :: Tile -> String
  }

suffixLenses ''GridStyle
suffixLenses ''BorderStyle

drawGrid :: GridStyle -> Widget name
drawGrid = do
  width <- view gridWidthL
  height <- view gridHeightL
  drawTile <- view drawTileL
  rows <-
    for [1..height] $ \y -> do
      row <- for [1..width] $ \x -> do
        pure $ str $ drawTile (x, y)
      insertVBorders row

  insertHBorders rows


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
  cellWidth <- view cellSizeL
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
