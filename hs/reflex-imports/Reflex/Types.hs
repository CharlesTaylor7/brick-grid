{-# language
  RankNTypes
, FlexibleContexts
, TypeFamilies
, ConstraintKinds
#-}
module Reflex.Types
  ( Widget
  , PushMonad
  )
  where

import Reflex.Dom hiding (Widget)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified GHCJS.DOM.Types as DOM

type Widget t m
  =
  ( MonadIO m
  , MonadFix m
  , Reflex t
  , DomBuilder t m
  , PostBuild t m
  , NotReady t m
  , MonadHold t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  , PushMonad (PushM t)
  , RawElement (DomBuilderSpace m) ~ DOM.Element
  )

type PushMonad m
  =
  ( MonadIO m
  , MonadFix m
  )