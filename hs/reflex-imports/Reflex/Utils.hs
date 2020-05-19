module Reflex.Utils where

import Reflex.Dom
import Control.Monad.IO.Class (MonadIO(..))

import Data.Default (Default(..))


once
  :: (MonadIO m, TriggerEvent t m)
  => a
  -> m (Event t a)
once val = do
  (event, trigger) <- newTriggerEvent
  liftIO $ trigger val
  pure event

onceDyn
  :: (MonadIO m, TriggerEvent t m, Default a, MonadHold t m)
  => a
  -> m (Dynamic t a)
onceDyn val = once val >>= holdDyn def

switchEvent
  :: (Reflex t, MonadHold t m)
  => Event t (Event t a)
  -> m (Event t a)
switchEvent nested = switchDyn <$> holdDyn never nested

switchWidgetEvent
  :: (Reflex t, MonadHold t m, Adjustable t m)
  => Event t (m (Event t a))
  -> m (Event t a)
switchWidgetEvent nested = switchDyn <$> widgetHold (pure never) nested
