{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import Brick hiding (Widget)
import qualified Brick as Scroll (ViewportType(..))
import qualified Brick as Brick

import Brick.Widgets.Border.Style
import Brick.Grid

import qualified Graphics.Vty as V

import qualified Data.Text as T


main :: IO ()
main = defaultMain app ()

drawUI :: Widget
drawUI =
  viewport GridView Scroll.Both $
  cached GridView $
  drawGrid drawTile gridStyle
  where
    cellWidth = 4
    drawTile = (, "tile") . T.pack . show . uncurry (*)
    gridStyle = GridStyle
      { borderStyle = unicodeRounded
      , gridWidth =  100
      , gridHeight = 55
      , padding = PadLeft
      }

-- | Ticks mark passing of time
-- the app's custom event type
type Tick = ()

-- | Named resources
data Name = GridView
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget Name
type AppState = ()

-- App definition
app :: App AppState Tick Name
app = App
  { appDraw = \_ -> pure drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const $ forceAttrMap mempty
  }

scrollAmount :: Int
scrollAmount = 3

handleEvent :: BrickEvent Name Tick -> AppState -> EventM Name (Next AppState)
handleEvent (VtyEvent (V.EvKey key [])) =
  case key of
    V.KEsc -> halt

    V.KChar 'h' -> \s -> do
      hScrollBy (viewportScroll GridView) (-scrollAmount)
      continue s

    V.KChar 'l' -> \s -> do
      hScrollBy (viewportScroll GridView) (scrollAmount)
      continue s

    V.KChar 'j' -> \s -> do
      vScrollBy (viewportScroll GridView) (scrollAmount)
      continue s

    V.KChar 'k' -> \s -> do
      vScrollBy (viewportScroll GridView) (-scrollAmount)
      continue s

    _ -> continue
handleEvent _ = continue
