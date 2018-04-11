{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Host.Basic (
    BasicGuest
  , basicHostWithQuit
  , basicHostForever
  , repeatUntilQuit
  ) where

import Control.Monad (void, when, unless, forM_, forM)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes, isJust)

import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.Fix (MonadFix)
import Data.IORef (newIORef, readIORef, writeIORef)

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar)
import Control.Concurrent.STM.TMVar (newEmptyTMVar, takeTMVar, putTMVar)

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

basicHostForever :: (forall t m. BasicGuest t m a)
                 -> IO a
basicHostForever guest = basicHostWithQuit $ (\x -> (x, never)) <$> guest

basicHostWithQuit :: (forall t m. BasicGuest t m (a, Event t ()))
                  -> IO a
basicHostWithQuit guest = do
  events <- liftIO newChan

  rHasQuit <- liftIO $ newIORef False

  ((a, eQuit), FireCommand fire) <- liftIO $ runSpiderHost $ do
    (((a, eQuit), postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      pae <- runTriggerEventT (runPostBuildT guest postBuild) events
      pure (pae, postBuildTriggerRef)

    hQuit <- subscribeEvent eQuit

    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> do
      lmQuit <- fire [postBuildTrigger :=> Identity ()] $ readEvent hQuit >>= sequence
      when (any isJust lmQuit) $
        liftIO $ writeIORef rHasQuit True

    pure ((a, eQuit), fc)

  done <- liftIO . atomically $ newEmptyTMVar
  let
    loop = do
      hasQuit <- liftIO $ readIORef rHasQuit
      if hasQuit
      then liftIO . atomically $ putTMVar done ()
      else do
        ers <- readChan events
        _ <- runSpiderHost $ do
          hQuit <- subscribeEvent eQuit
          mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation x _) ->
            fmap (\e -> e :=> Identity x) <$> readIORef er

          lmQuit <- fire (catMaybes mes) $ readEvent hQuit >>= sequence
          when (any isJust lmQuit) $
            liftIO $ writeIORef rHasQuit True

          liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
        loop

  void . liftIO . forkIO $ loop
  void . liftIO . atomically . takeTMVar $ done

  pure a

repeatUntilQuit :: Event t () -> IO a -> BasicGuest t m ()
repeatUntilQuit eQuit act = do
  ePostBuild <- getPostBuild
  tHasQuit <- liftIO . atomically $ newTVar False

  let
    loop = do
      hasQuit <- liftIO . atomically $ readTVar tHasQuit
      unless hasQuit $ do
        void act
        loop

  performEvent_ $ liftIO (void . forkIO $ loop) <$ ePostBuild
  performEvent_ $ liftIO (atomically $ writeTVar tHasQuit True) <$ eQuit

  pure ()

