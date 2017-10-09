{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Basic.Host (
    BasicGuest
  , basicHost
  ) where

import Control.Monad (void, forever, forM_, forM)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes)

import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.Fix (MonadFix)
import Data.IORef (readIORef)

import Data.Dependent.Sum
import Reflex
import Reflex.Host.Class

type BasicGuest t m a =
  ( MonadReflexHost t m
  , MonadHold t m
  , Ref m ~ Ref IO
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  , PrimMonad (HostFrame t)
  , MonadIO m
  , MonadFix m
  ) => PostBuildT t (TriggerEventT t (PerformEventT t m)) a

basicHost :: (forall t m. BasicGuest t m a) -> IO a
basicHost guest = do
  events <- liftIO newChan

  (a, fc@(FireCommand fire)) <- liftIO $ runSpiderHost $ do
    ((a, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      a <- runTriggerEventT (runPostBuildT guest postBuild) events
      pure (a, postBuildTriggerRef)
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    pure (a, fc)

  void . liftIO . forkIO . forever $ do
    ers <- readChan events
    _ <- runSpiderHost $ do
      mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
        me <- readIORef er
        return $ fmap (\e -> e :=> Identity a) me
      _ <- fire (catMaybes mes) $ return ()
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
    pure ()

  pure a

