{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Grid
  ( GridStyle(..)
  , Padding(..)
  , drawGrid
  ) where

import GHC.Generics (Generic)
import Data.Traversable (for)
import Data.List (intersperse)

import Control.Monad.Reader (ask)

import Data.Text (Text)
import qualified Data.Text as T

import Lens.Micro (Lens', (&), (<&>), to, (^.))
import Lens.Micro.Mtl (view)

import Brick (Widget, AttrName, withAttr, hBox, vBox, txt, textWidth)
import Brick.Widgets.Border.Style (BorderStyle)

import Brick.Grid.TH (suffixLenses)


data Padding = PadLeft | PadRight

data GridStyle = GridStyle
  { borderStyle :: BorderStyle
  -- ^ Borderstyle to use. Brick.Widgets.Border.Style exports unicode, unicodeRounded, unicodeBolded, & ascii out of the box. Other border styles can be created or modified with the BorderStyle constructor.
  , gridWidth :: Int
  -- ^ number of cells in each row
  , gridHeight :: Int
  -- ^ number of cells in each column
  , padding :: Padding
  }
  deriving (Generic)

suffixLenses ''GridStyle
suffixLenses ''BorderStyle

drawGrid
  :: ((Int, Int) -> (Text, AttrName))
  -- ^ tile drawing function
  -> GridStyle
  -- ^ grid style options
  -> Widget name
drawGrid drawTileF = do
  width  <- view gridWidthL
  height <- view gridHeightL
  padding <- view paddingL
  let
    columnIndices = [0 .. width - 1]
    rowIndices    = [0 .. height - 1]

    columnWidth i = rowIndices
      & map (\j -> textWidth $ fst $ drawTileF (i, j))
      & maximum

    columnWidths = map columnWidth columnIndices

  rows <-
    for rowIndices $ \j ->
      insertVBorders $
        columnIndices <&>
          (\i ->
            let (content, name) = drawTileF (i, j)
            in content
              & pad padding (columnWidths !! i)
              & txt
              & withAttr name
          )

  insertHBorders columnWidths rows


pad :: Padding -> Int -> Text -> Text
pad p n t =
  case n - T.length t of
    d | d > 0 ->
      case p of
        PadLeft  -> T.replicate d " " <> t
        PadRight -> t <> T.replicate d " "
      | otherwise -> t


insertVBorders :: [Widget name] -> GridStyle -> Widget name
insertVBorders cells = do
  v <- view $ borderStyleL . bsVerticalL . to (txt . T.singleton)
  pure . hBox . (v :) . (<> [v]) . intersperse v $ cells

insertHBorders :: [Int] -> [Widget name] -> GridStyle -> Widget name
insertHBorders columnWidths cells = do
  hBorder <- pure $ hBorder columnWidths
  h1 <- hBorder Top
  h2 <- hBorder Bottom
  h3 <- hBorder Middle
  pure . vBox . (h1 :) . (<> [h2]) . intersperse h3 $ cells


hBorder :: [Int] -> VLocation -> GridStyle -> Widget name
hBorder columnWidths v = do
  innerBorder <- view $ borderStyleL . borderStyleLens v Center
  startCorner <- view $ borderStyleL . borderStyleLens v Start
  endCorner <- view $ borderStyleL . borderStyleLens v End
  pipe <- view $ borderStyleL . bsHorizontalL

  columnWidths
    & map (\w -> T.replicate w (T.singleton pipe))
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
