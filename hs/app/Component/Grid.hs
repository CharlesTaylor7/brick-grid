module Component.Grid
  ( grid
  , gridDynStyle
  ) where

import Reflex hiding (elDynClass)
import Types

import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals


gridDynStyle
  :: (DomBuilder t m, PostBuild t m)
  => Generals.Map t
  -> Dynamic t Style
  -> m ()
gridDynStyle map gridStyle = do
  let mapHeight = map ^. map_height
  let mapWidth  = map ^. map_width

  elDynStyle "table" (gridStyle <&> style_cssClass .~ Class "grid") $
    elClass "tbody" "" $
      for_ [1..mapHeight] $ \j ->
      elClass "tr" "" $
        for_ [1..mapWidth] $ \i ->
        tileElement map (i, j)

grid
  :: (DomBuilder t m, PostBuild t m)
  => Generals.Map t
  -> m ()
grid map = gridDynStyle map def

tileElement
  :: forall t m. (DomBuilder t m, PostBuild t m)
  => Generals.Map t
  -> (Int, Int)
  -> m ()
tileElement map coords =
  let
    gridIx :: (Int, Int) -> Traversal' Grid Tile
    gridIx = ix . linearize
      where
        linearize (i, j) = (j - 1) * mapWidth + (i - 1)
        mapWidth = map ^. map_width

    tileTraversal :: Traversal' Grid Tile
    tileTraversal = gridIx coords

    nonzero :: (Eq a, Num a) => Prism' a a
    nonzero = from (non 0) . _Just

    dynClass :: Dynamic t CSSClass
    dynClass = map ^. map_tiles
      <&> (preview tileTraversal >>> toClass)

    dynArmyText :: Dynamic t Text
    dynArmyText = map ^. map_tiles
      <&>
        (   preview (tileTraversal . _Army . army_size . nonzero . to show)
        >>> maybe "" identity
        )
  in
    elDynClass (Dom.Node "td") dynClass $ dynText dynArmyText


toClass :: Maybe Tile -> CSSClass
toClass tile =
  case tile of
    Just (Clear _) ->    Class "clear"
    Just (City _) ->     Class "city"
    Just (General _) ->  Class "general"
    Just (Swamp _) ->    Class "swamp"
    Just Mountain ->     Class "mountain"
    Just Fog_Clear ->    Class "fog-clear"
    Just Fog_Obstacle -> Class "fog-obstacle"
    _ ->                 Class "unknown"
