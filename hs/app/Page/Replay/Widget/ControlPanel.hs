module Page.Replay.Widget.ControlPanel
  ( controlPanel
  )
  where

import Reflex

import Data.Dom

import Page.Replay.Simulate
import Page.Replay.Download (downloadReplay)
import Page.Replay.Utils (getCachedReplays)
import Page.Replay.Types

import Component.Elastic
import Component.Grid

import Js.Imports (JSVal, asyncCallback1, (!), fromJSValUnchecked)
import Js.Types
import Js.Utils
import qualified Js.FFI as FFI


controlPanel :: Widget t m => Dynamic t Turn -> m (Event t ReplayLocation, Dynamic t Turn)
controlPanel dynMaxTurn = do
  cachedReplays :: Event t [ReplayLocation] <-
    getCachedReplays

  replayLocationEv :: Event t ReplayLocation <-
    replayDropdown cachedReplays

  replayUrlHref replayLocationEv

  jumpToTurnEvent :: Event t Command <-
    jumpToTurnInputEl

  keyEvent :: Event t Command <-
    registerKeyCommands

  -- fold commands into dynamic turn
  let commandEvent = keyEvent <> jumpToTurnEvent

  dynTurn :: Dynamic t Turn <-
    buildDynTurn commandEvent dynMaxTurn

  elClass "div" "turn-marker" $
    dynText (dynTurn <&> ("turn: " <>) . show . halfRoundUp . view _Turn)

  pure (replayLocationEv, dynTurn)


replayDropdown :: Widget t m => Event t [ReplayLocation] -> m (Event t ReplayLocation)
replayDropdown cachedReplays =
  bindEventToWidget cachedReplays $
  \replays -> do
    let
      optionsVector :: Vector ReplayLocation
      optionsVector = fromList replays

      optionsMap :: Map Int Text
      optionsMap = fromList $ replays ^.. ifolded . to toDescription . withIndex

      initialKey = -1

      lookup i = optionsVector ^? ix i
    dropdown initialKey (pure optionsMap) def
      <&> fmapMaybe lookup . _dropdown_change


replayUrlHref :: Widget t m => Event t ReplayLocation -> m ()
replayUrlHref i = widgetHold_ blank $ i <&>
  \replay ->
  elAttr "a"
    ( mempty
    & at "href" ?~ "http://generals.io/replays/" <> replay ^. replayLocation_id
    & at "target" ?~ "_blank"
    ) $
    text "Replay"


jumpToTurnInputEl :: Widget t m => m (Event t Command)
jumpToTurnInputEl = do
  turnInput <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~
      ( at (toAttr "type") ?~ "text") .
      ( at (toAttr "pattern") ?~ "[0-9]*")
      -- ( at (toAttr "autofocus") ?~ "")

  let
    inputEvent = mapMaybe
      (preview $ unpacked . to readEither . _Right . re (_JumpTo . _Turn))
      (turnInput ^. inputElement_input)

  pure inputEvent

  where
    toAttr :: Text -> AttributeName
    toAttr = AttributeName Nothing


registerKeyCommands :: Widget t m => m (Event t Command)
registerKeyCommands = do
-- Register j & k key commands
  (rawEvent, trigger) <- newTriggerEvent

  jsCallback <- liftIO $ asyncCallback1 trigger
  liftIO $ FFI.registerOnKeydown jsCallback

  (performEvent $ rawEvent <&> toKey) <&> mapMaybe toCommand


buildDynTurn :: forall t m. (Reflex t, MonadHold t m, MonadFix m) => Event t Command -> Dynamic t Turn -> m (Dynamic t Turn)
buildDynTurn commandEvent dynMaxTurn = do
  let
    maxTurnOrCommandEv :: Event t (Turn, Command)
    maxTurnOrCommandEv = leftmost
      [ attach (current dynMaxTurn) commandEvent
      , updated dynMaxTurn & fmapCheap (,DoNothing)
      ]
  foldDyn (commandReducer def) def $ maxTurnOrCommandEv

toDescription :: ReplayLocation -> Text
toDescription (ReplayLocation server id) =
  describe server
  <> "-"
  <> id
  where
    describe Server_Main = "main"
    describe Server_Bot = "bot"


halfRoundUp :: Int -> Int
halfRoundUp = uncurry (+) . (`divMod` 2)

toKey :: MonadIO m => JSVal -> m KeyCode
toKey jsval = liftIO $ do
  keyCode <- jsval ! ("keyCode" :: Text)
  fromJSValUnchecked keyCode

toCommand :: KeyCode -> Maybe Command
toCommand code =
  case keyCodeLookup code of
    KeyJ -> Just Backwards
    KeyL -> Just Forwards
    _    -> Nothing

commandReducer :: Turn -> (Turn, Command) -> Turn -> Turn
commandReducer minTurn (maxTurn, command) =
  case command of
    DoNothing -> min maxTurn
    Backwards -> max minTurn . min maxTurn . subtract 1
    Forwards  -> min maxTurn  . (+ 1)
    JumpTo n  -> const $ max minTurn $ min maxTurn n

theseToDefaults :: (Default a, Default b) => These a b -> (a, b)
theseToDefaults (This a)    = (a  , def)
theseToDefaults (That b)    = (def, b  )
theseToDefaults (These a b) = (a  , b  )
