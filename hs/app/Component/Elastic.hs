module Component.Elastic
  ( elastic
  ) where

import Prelude hiding (subtract)
import Reflex hiding (elDynClass)
import Types

import Data.Group
import Data.These
import Data.Dom
import qualified Data.Dom as Dom

import Generals.Map.Types hiding (Map)
import qualified Generals.Map.Types as Generals

import Component.Elastic.Types hiding (Point)
import qualified Component.Elastic.Types as E

type Point = E.Point Double
pattern Point x y = E.Point x y
{-# complete Point #-}

elastic
  :: forall t m a.
  (  Widget t m
  )
  => InitialDimensions
  -> (Dynamic t Style -> m a)
  -> m a
elastic dims child = do
  let
    elasticStyle :: Style
    elasticStyle = def
      & style_cssClass .~ Class "elastic"
      & style_inline .~ (
        def
        & at "width" ?~ "100vw"
        & at "height" ?~ "100vh"
      )
  rec
    (e, a) <- elStyle' "div" elasticStyle $ child dynChildStyle
    dynChildStyle <- getTransform e <&> fmap (toStyle dims)

  pure a

getTransform :: forall t m. Widget t m => Element t m -> m (Dynamic t Transform)
getTransform e = do
  dragging :: Behavior t Dragging <-
    hold DragOff $ fold
      [ mouseDown  $> DragOn
      , mouseUp    $> DragOff
      , mouseLeave $> DragOff
      ]

  mousePosition :: Behavior t Point <-
    hold zero mouseMove

  zoomScale :: Dynamic t Double <-
    wheelEvent
    & accumDyn zoomReducer 0
    -- range from quarter size to 4 times as big
    <&> fmap (\level -> 1 + 0.001 * level)


  dragReferencePointDyn :: Dynamic t Point <-
    accumDyn (&) zero $ leftmost
      [ mouseDown
        & traceEventWith (\p -> "MouseDown: " <> show p)
        & fmapCheap subtract

      , mouseUp
        & gate (coerceBehavior dragging)
        & traceEventWith (\p -> "MouseUp: " <> show p)
        & fmapCheap add

      , mouseLeave
        & gate (coerceBehavior dragging)
        & traceEventWith (\p -> "MouseLeave: " <> show p)
        & attachWith (const . add) mousePosition
      ]
  let dragReferencePoint = current dragReferencePointDyn

  display dragReferencePointDyn

  dynOffset :: Dynamic t Point <-
    holdDyn zero $ leftmost
      [ mouseMove
        & gate (coerceBehavior dragging)
        & attachWith add dragReferencePoint
      , zoomScale
        & updated
        & traceEventWith (\zoom -> "Zoom: " <> show zoom)
        <&> \(!_) -> mempty
        -- & attachWith (\mouse zoom -> add mouse . scale zoom . subtract mouse) mousePosition
      ]

  let
    dynTransform :: Dynamic t Transform
    dynTransform = zipDynWith Transform dynOffset zoomScale

  pure dynTransform
  where
    zoomReducer a b = a + b & clamp (-750) 3000

    wheelEvent :: Event t Double
    wheelEvent = domEvent Wheel e & fmapCheap _wheelEventResult_deltaY

    mouseMove, mouseDown, mouseUp :: Event t Point
    mouseMove = mouseEvent Mousemove e
    mouseDown = mouseEvent Mousedown e
    mouseUp   = mouseEvent Mouseup   e

    mouseLeave :: Event t ()
    mouseLeave = domEvent Mouseleave e


clamp :: Ord n => n -> n -> n -> n
clamp min max n
  | n < min = min
  | n > max = max
  | otherwise = n

mouseEvent
  ::
  (  Reflex t
  ,  HasDomEvent t target eventName
  ,  DomEventType target eventName ~ (Int, Int)
  )
  => EventName eventName
  -> target
  -> Event t Point
mouseEvent tag e = domEvent tag e & coerceEvent & fmapCheap toDoublePoint & coerceEvent

toDoublePoint :: (Int, Int) -> Point
toDoublePoint (x, y) = Point (fromIntegral x) (fromIntegral y)