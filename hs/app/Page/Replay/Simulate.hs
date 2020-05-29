module Page.Replay.Simulate
 ( toHistory
 )
 where

import Page.Replay.Simulate.Types
import Page.Replay.Types
import Generals.Map.Types hiding (Map)

import Prelude hiding (scanl)
import Data.Vector (Vector, scanl')
import Data.Default

import Types (Dimensions, width, height)

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Vector (Vector)


toHistory :: Replay -> Vector Grid
toHistory replay = view gameInfo_grid <$>
  scanl'
    (flip $ advanceTurn)
    (initialGameInfo replay)
    (fromList . turns $ replay ^. moves)

turns :: [Move] -> Turns
turns moves = unfoldr f (1, moves)
  where
    f (i, moves) =
      if null moves
      then Nothing
      else Just $
        let (group, rest) = moves & span ((i ==) . view turn)
        in ((i, group), (i + 1, rest))

initialGameInfo :: Replay -> GameInfo
initialGameInfo replay = GameInfo
  { _gameInfo_grid = map
  , _gameInfo_activeCities = fromList $ replay ^.. generals . folded
  , _gameInfo_activeSwamps = mempty
  , _gameInfo_owned = fromList $ replay ^.. generals . ifolded . withIndex  . alongside identity (to singleton)
  }
  where
    singleton :: Int -> IntSet
    singleton tile = mempty & contains tile .~ True
    map = fold
      [ mountainsMap
      , citiesMap
      , generalsMap
      , swampsMap
      , clearMap
      ]
    mountainsMap = fromList $
      [ (index, Mountain)
      | index <- replay ^.. mountains . folded
      ]

    citiesMap = fromList $
      [ (index, City (Neutral `Army` fromIntegral size))
      | (index, size) <- zip (replay ^.. cities . folded) (replay ^.. cityArmies . folded)
      ]

    generalsMap = fromList $
      [ (boardIndex, General $ Player playerId `Army` 1)
      | (playerId, boardIndex) <- replay ^.. generals . folded . withIndex
      ]

    swampsMap = fromList $
      [ (boardIndex, Swamp def)
      | boardIndex <- replay ^.. swamps . folded
      ]

    clearMap = fromList $
      [ (i, def)
      | i <- [0..numTiles - 1]
      ]
    numTiles = replay^.mapWidth * replay^.mapHeight

advanceTurn :: Turn -> GameInfo -> GameInfo
advanceTurn (turnIndex, moves) =
  cityGrowth turnIndex .
  tileGrowth turnIndex .
  (execState $ swampLoss turnIndex) .
  applyMoves moves

increment i = singular (ix i . _Army . size) +~ 1

cityGrowth :: Int -> GameInfo -> GameInfo
cityGrowth turnIndex info =
  if turnIndex `mod` 2 == 1
  then
    info
    & gameInfo_grid .~
      foldl'
        (flip increment)
        (info ^. gameInfo_grid)
        (info ^. gameInfo_activeCities . to Set.toList)
  else
    info

tileGrowth :: Int -> GameInfo -> GameInfo
tileGrowth turnIndex info =
  if turnIndex `mod` 50 == 49
  then
    info
    & gameInfo_grid .~
      foldl'
        (flip increment)
        (info ^. gameInfo_grid)
        (info ^. gameInfo_owned . folded . to Set.toList)
  else
    info

swampLoss :: MonadState GameInfo m => Int -> m ()
swampLoss turnIndex =
  when (turnIndex `mod` 2 == 1) $ do
    swamps <- use $ gameInfo_activeSwamps . to Set.toList
    for_ swamps $ \i -> do
      updatedArmy <- gameInfo_grid . ixLens i . singular _Army <%=
        \army ->
          if army^.size > 1
          then army & size -~ 1
          else Neutral `Army` 0
      when (is _Neutral $ updatedArmy ^. owner) $
        gameInfo_activeSwamps . contains i .= False

applyMoves :: [Move] -> GameInfo -> GameInfo
applyMoves moves info =
  traverse_ moveReducer moves
  &   runWriterT
  >>= traverse_ applyKill . view _2
  &   flip execState info

ixLens :: (Ixed s) => Index s -> Lens' s (IxValue s)
ixLens = singular . ix

applyKill :: MonadState GameInfo m => Kill -> m ()
applyKill (Kill killer target) = do
  -- remove all territory belonging to target
  territory <- gameInfo_owned . ixLens target <<.= mempty

  -- give it to killer
  gameInfo_owned . ixLens killer <>= territory

  -- halve the armies in the transferred territory
  for_ (Set.toList territory) $ \i ->
    gameInfo_grid . ix i . _Army . size %= halfRoundUp



moveReducer
  :: (MonadState GameInfo m, MonadWriter [Kill] m)
  => Move
  -> m ()
moveReducer move = do
  let
    attackingTile move = ixGrid (move ^. startTile)
    defendingTile move = ixGrid (move ^. endTile)

  -- subtract army from attacking tile
  tileArmy <-
    gameInfo_grid . attackingTile move . singular _Army
    <<%= over size (leaveArmySize $ move ^. onlyHalf)

  -- build attacking army
  let
    attackingPlayer = tileArmy ^. owner
    attackingArmySize = moveArmySize (move ^. onlyHalf) (tileArmy ^. size)
    attackingArmy = attackingPlayer `Army` attackingArmySize

  -- determine defending player
  defendingPlayer <- use (gameInfo_grid . defendingTile move . singular _Army . owner)

  -- army leftover after attack
  newArmy <-
    gameInfo_grid . defendingTile move . singular _Army
    <%= attack attackingArmy

  let newOwner = newArmy ^. owner
  defendingTileWasGeneral <- use (gameInfo_grid . ixGrid (move ^. endTile) . to (is _General))

  -- update cache
  when (newOwner /= defendingPlayer) $ do
    let newPlayerId = newOwner ^?! _Player
    gameInfo_owned . ixLens newPlayerId . contains (move ^. endTile . _GridIndex) .= True
    case defendingPlayer ^? _Player of
      Just id ->
        gameInfo_owned . ixLens id . contains (move ^. endTile . _GridIndex) .= False
      _ -> do
        defenseTileType <- use (gameInfo_grid . ixGrid (move ^. endTile) . armyTileType)
        when (defenseTileType == Swamp_Tile) $
          gameInfo_activeSwamps . contains (move ^. endTile . _GridIndex) .= True
        when (defenseTileType == City_Tile) $
          gameInfo_activeCities . contains (move ^. endTile . _GridIndex) .= True

  -- emit kill & convert conquered tile to being a city
  when (newOwner /= defendingPlayer && defendingTileWasGeneral) $ do
    tell $ pure $ Kill
      { kill_killer = newOwner ^?! _Player
      , kill_target = defendingPlayer ^?! _Player
      }
    gameInfo_grid . ixGrid (move ^. endTile) . armyTileType .= City_Tile

  pure ()

halfRoundUp :: Integral n => n -> n
halfRoundUp = uncurry (+) . (`divMod` 2)

halfRoundDown :: Integral n => n -> n
halfRoundDown = (`div` 2)

leaveArmySize :: Integral n => Bool -> n -> n
leaveArmySize onlyHalf =
  if onlyHalf
  then halfRoundDown
  else const 1

moveArmySize :: Integral n => Bool -> n -> n
moveArmySize onlyHalf =
  if onlyHalf
  then halfRoundUp
  else subtract 1


attack :: Army -> Army -> Army
attack attacking defending
  | attacking ^. owner == defending ^. owner
  = defending & size +~ attacking ^. size

  | attacking ^. size > defending ^. size
  = attacking & size -~ defending ^. size

  | otherwise
  = defending & size -~ attacking ^. size