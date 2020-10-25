{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Brick.Grid
  ( Tile
  , GridStyle(..)
  , drawGrid
  )
  where

import GHC.Generics (Generic)
import Data.Traversable (for)
import Data.List (intercalate, intersperse)

import Lens.Micro (Lens', (&), to)
import Lens.Micro.Mtl (view)
import Data.Generics.Labels ()

import Brick (Widget, str, vBox, hBox)
import Brick.Widgets.Border.Style (BorderStyle)


type Tile = (Int, Int)

data GridStyle = GridStyle
  { borderStyle :: BorderStyle
  , cellSize :: Int
  , gridWidth :: Int
  , gridHeight :: Int
  , drawTile :: Tile -> String
  }
  deriving (Generic)


drawGrid :: GridStyle -> Widget name
drawGrid = do
  width <- view #gridWidth
  height <- view #gridHeight
  drawTile <- view #drawTile
  rows <-
    for [1..height] $ \y -> do
      row <- for [1..width] $ \x -> do
        pure $ str $ drawTile (x, y)
      insertVBorders row

  insertHBorders rows


insertVBorders :: [Widget name] -> GridStyle -> Widget name
insertVBorders cells = do
  v <- view $ #borderStyle . #bsVertical . to (str . pure)
  pure . hBox . (v:) . (<> [v]) . intersperse v $ cells

insertHBorders :: [Widget name] -> GridStyle -> Widget name
insertHBorders cells = do
  h1 <- hBorder Top
  h2 <- hBorder Bottom
  h3 <- hBorder Middle
  pure . vBox . (h1 :) . (<> [h2]) . intersperse h3 $ cells


hBorder :: VLocation -> GridStyle -> Widget name
hBorder v = do
  cellWidth <- view #cellSize
  mapWidth <- view #gridWidth
  innerBorder <- view $ #borderStyle . borderStyleL v Center
  startCorner <- view $ #borderStyle . borderStyleL v Start
  endCorner <- view $ #borderStyle . borderStyleL v End
  pipe <- view $ #borderStyle . #bsHorizontal
  replicate cellWidth pipe
      & replicate mapWidth
      & intercalate [innerBorder]
      & \row -> [startCorner] <> row <> [endCorner]
      & str
      & pure

data VLocation = Bottom | Middle | Top
data HLocation = Start | Center | End

borderStyleL :: VLocation -> HLocation -> Lens' BorderStyle Char
borderStyleL Bottom Start  = #bsCornerBL
borderStyleL Bottom Center = #bsIntersectB
borderStyleL Bottom End    = #bsCornerBR
borderStyleL Middle Start  = #bsIntersectL
borderStyleL Middle Center = #bsIntersectFull
borderStyleL Middle End    = #bsIntersectR
borderStyleL Top Start     = #bsCornerTL
borderStyleL Top Center    = #bsIntersectT
borderStyleL Top End       = #bsCornerTR
