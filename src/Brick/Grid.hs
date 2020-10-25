module Brick.Grid where

import Prelude hiding (Empty, on)
import Types
import Control.Lens.Unsafe

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick as Scroll (ViewportType(..))
import qualified Brick as Brick

import Brick.BChan (newBChan, writeBChan)
-- import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

import qualified Graphics.Vty as V

-- | show army count in 4 characters
-- if the army is over 4 digits, use k prefix
-- if the army is under 4 digits pad with space characters
showArmyCount :: Int -> String
showArmyCount n
  | n < 10 = "  " <> show n <> " "
  | n < 100 = " " <> show n <> " "
  | n < 1_000 = show n <> " "
  | n < 10_000 = show n
  | n < 100_000 = " " <> show (n `div` 1_000) <> "k"
  | n < 1_000_000 = show (n `div` 1_000) <> "k"
  | otherwise = "lorj"


gridAttrMap = attrMap V.defAttr $
  players <>
    [ ("obstacle", fg grey)
    , ("general", V.currentAttr `V.withStyle` V.standout)
    , ("city", V.currentAttr `V.withStyle` V.underline)
    ]
  where
    grey = V.rgbColor 0x71 0x6f 0x6f
    players =
      zipWith
        (\attr i -> (attrName $ "player" <> show i, attr))
        playerAttributes
        [1..]

playerAttributes =
  [ fg V.brightBlue
  , fg V.red
  , fg V.green
  , fg purple
  , fg teal
  , fg V.magenta
  , fg V.cyan
  , fg orange
  ]
  where
    teal = V.rgbColor 0 0x80 0x80
    purple = V.rgbColor 0x80 0 0x80
    orange = V.rgbColor 0xea 0x45 0x11

brickMain :: History -> IO ()
brickMain history = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100_000

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) app (history, TurnIndex 0)

  pure ()

-- Types

-- | Ticks mark passing of time
--
-- the app's custom event type
data Tick = Tick

-- | Named resources
data Name = GridView
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget Name

data Cell
  = Snake
  | Food
  | Empty
  deriving (Eq, Show)

type AppState = (History, TurnIndex)

-- App definition
app :: App AppState Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = flip handleEvent
  , appStartEvent = pure
  , appAttrMap = const gridAttrMap
  }

handleEvent :: BrickEvent Name Tick -> AppState -> EventM Name (Next AppState)
handleEvent (AppEvent Tick) = continue
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

scrollAmount = 5

drawUI :: AppState -> [Widget]
drawUI (history, TurnIndex turn) =
  [ center $ flip runReader gridStyle $ drawGrid game
  ]
  where
    game = history ^?! ix turn $ "history index"
    gridStyle = GridStyle
      { borderStyle = unicode
      , cellSize = 4
      , gridWidth = game ^. #replay . #mapWidth
      , gridHeight = game ^. #replay . #mapHeight
      }
data GridStyle = GridStyle
  { borderStyle :: BorderStyle
  , cellSize :: Int
  , gridWidth :: Int
  , gridHeight :: Int
  }
  deriving (Generic)


drawTitle :: GameInfo -> Widget
drawTitle gameInfo = do
  let
    dimensions = show width <> "x" <> show height
    width = gameInfo ^. #replay . #mapWidth
    height = gameInfo ^. #replay . #mapHeight

  hCenter $ str $ "Replay " <> dimensions


drawGrid :: MonadReader GridStyle m => GameInfo -> m Widget
drawGrid gameInfo = do
  let
    width = gameInfo ^. #replay . #mapWidth
    height = gameInfo ^. #replay . #mapHeight

  rows <-
    for [1..height] $ \y -> do
      row <- for [1..width] $ \x -> do
        let
          tile = gameInfo ^. #grid . ixGrid i
          i = GridIndex $ (y - 1) * width + (x - 1)
        drawTile tile
      insertVBorders row

  gridContent <- insertHBorders rows

  let
    grid :: Widget
    grid =
      viewport GridView Scroll.Both $
      cached GridView $
        gridContent

  pure $ drawTitle gameInfo <=> grid

-- | Draw tile
drawTile :: MonadReader GridStyle m => Tile -> m Widget
drawTile tile = do
  cellWidth <- view #cellSize
  pure
    $ str (tile ^. contents cellWidth)
    & withAttr (tile ^. ownerAttr <> tile ^. to terrainAttr)
  where
    ownerAttr :: Fold Tile AttrName
    ownerAttr =
      (_Owner . #_Player . to (+1) . to show . to ("player" <>) . to attrName)
      `failing`
      like "neutral"

    contents w =
      (_Army . #size . from (non 0) . _Just . to showArmyCount)
      `failing`
      like (replicate w ' ')

terrainAttr :: Tile -> AttrName
terrainAttr (Clear _) = "clear"
terrainAttr (City _) = "city"
terrainAttr (General _) = "general"
terrainAttr (Swamp _) = "swamp"
terrainAttr Mountain  = "mountain"
terrainAttr Fog_Clear  = "fog"
terrainAttr Fog_Obstacle  = "fog"

insertVBorders :: MonadReader GridStyle m => [Widget] -> m Widget
insertVBorders cells = do
  v <- view $ #borderStyle . #bsVertical . to (str . pure)
  pure . hBox . (v:) . (<> [v]) . intersperse v $ cells

insertHBorders :: MonadReader GridStyle m => [Widget] -> m Widget
insertHBorders cells = do
  h1 <- hBorder Top
  h2 <- hBorder Bottom
  h3 <- hBorder Middle
  pure . vBox . (h1 :) . (<> [h2]) . intersperse h3 $ cells


hBorder :: MonadReader GridStyle m => VLocation -> m Widget
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
data Pipe = Horizontal | Vertical

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
