{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Grid
  ( GridStyle(..)
  , drawGrid
  , padOrTruncate
  , toTileWidget
  ) where

import GHC.Generics (Generic)
import Data.Traversable (for)
import Data.List (intersperse, intercalate)

import Control.Monad.Reader (ask)

import Data.Text (Text)
import qualified Data.Text as T

import Lens.Micro (Lens', (&), (<&>), to)
import Lens.Micro.Mtl (view)

import Brick (Widget, AttrName, withAttr, hBox, vBox, txt)
import Brick.Widgets.Border.Style (BorderStyle)

import Brick.Grid.TH (suffixLenses)


padOrTruncate :: Int -> Text -> Text
padOrTruncate n t =
  case n - T.length t of
    0 -> t
    p | p > 0     -> T.replicate p " " <> t
      | otherwise -> T.take n t


toTileWidget :: Int -> AttrName -> Text -> Widget name
toTileWidget cellWidth attrName text =
  withAttr attrName $
  txt $ padOrTruncate cellWidth text

data GridStyle name = GridStyle
  { borderStyle :: BorderStyle
  -- ^ Borderstyle to use. Brick.Widgets.Border.Style exports unicode, unicodeRounded, unicodeBolded, & ascii out of the box. Other border styles can be created or modified with the BorderStyle constructor.
  , gridWidth :: Int
  -- ^ number of cells in each row
  , gridHeight :: Int
  -- ^ number of cells in each column
  , cellWidth :: Int
  -- ^ text width of each cell
  , drawTileWidget :: (Int, Int) -> Widget name
  -- ^. tile drawing function. It is up to you to ensure all tile widgets rendered width and is the same as the chosen cellWidth.
  }
  deriving (Generic)


suffixLenses ''GridStyle
suffixLenses ''BorderStyle


drawGrid :: GridStyle name -> Widget name
drawGrid = do
  width  <- view gridWidthL
  height <- view gridHeightL
  let
    rowIndices    = [0 .. width - 1]
    columnIndices = [0 .. height - 1]

  drawTile <- view drawTileWidgetL
  rows <-
    for columnIndices $ \y ->
      insertVBorders $
        rowIndices <&> (\x -> drawTile (x, y))

  insertHBorders rows

insertVBorders :: [Widget name] -> GridStyle name -> Widget name
insertVBorders cells = do
  v <- view $ borderStyleL . bsVerticalL . to (txt . T.singleton)
  pure . hBox . (v :) . (<> [v]) . intersperse v $ cells

insertHBorders :: [Widget name] -> GridStyle name -> Widget name
insertHBorders cells = do
  h1 <- hBorder Top
  h2 <- hBorder Bottom
  h3 <- hBorder Middle
  pure . vBox . (h1 :) . (<> [h2]) . intersperse h3 $ cells


hBorder :: VLocation -> GridStyle name -> Widget name
hBorder v = do
  cellWidth <- view cellWidthL
  mapWidth <- view gridWidthL
  innerBorder <- view $ borderStyleL . borderStyleLens v Center
  startCorner <- view $ borderStyleL . borderStyleLens v Start
  endCorner <- view $ borderStyleL . borderStyleLens v End
  pipe <- view $ borderStyleL . bsHorizontalL
  T.replicate cellWidth (T.singleton pipe)
      & replicate mapWidth
      & T.intercalate (T.singleton innerBorder)
      & \row -> T.cons startCorner (T.snoc row endCorner)
      & txt
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
