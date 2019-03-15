{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Host.Basic (
    BasicGuest
  , BasicGuestConstraints
  , basicHostWithQuit
  , basicHostForever
  , repeatUntilQuit
  ) where

import Control.Monad (void, when, unless, forM_, forM)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes, isJust)

import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
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
import Reflex.NotReady.Class

type BasicGuestConstraints t (m :: * -> *) =
  ( MonadReflexHost t m
  , MonadHold t m
  , MonadSample t m
  , Ref m ~ Ref IO
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  , PrimMonad (HostFrame t)
  , MonadIO m
  , MonadFix m
  )

newtype BasicGuest t (m :: * -> *) a =
  BasicGuest {
    unBasicGuest :: PostBuildT t (TriggerEventT t (PerformEventT t m)) a
  } deriving (Functor, Applicative, Monad, MonadFix)

instance (MonadIO m, ReflexHost t, MonadIO (HostFrame t)) => MonadIO (BasicGuest t m) where
  liftIO = BasicGuest . liftIO

instance ReflexHost t => MonadSample t (BasicGuest t m) where
  {-# INLINABLE sample #-}
  sample = BasicGuest . lift . sample

instance (ReflexHost t, MonadHold t m) => MonadHold t (BasicGuest t m) where
  {-# INLINABLE hold #-}
  hold v0 = BasicGuest . lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = BasicGuest . lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = BasicGuest . lift . holdIncremental v0
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = BasicGuest . lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = BasicGuest . lift . headE

instance (Reflex t, ReflexHost t) => PostBuild t (BasicGuest t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = BasicGuest getPostBuild

instance (Reflex t, ReflexHost t, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO) => TriggerEvent t (BasicGuest t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = BasicGuest $ lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = BasicGuest $ lift newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete = BasicGuest . lift . newEventWithLazyTriggerWithOnComplete

instance (Reflex t, ReflexHost t, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO, MonadIO (HostFrame t), PrimMonad (HostFrame t), MonadIO m) => PerformEvent t (BasicGuest t m) where
  type Performable (BasicGuest t m) = HostFrame t
  {-# INLINABLE performEvent_ #-}
  performEvent_ = BasicGuest . lift . lift . performEvent_
  {-# INLINABLE performEvent #-}
  performEvent = BasicGuest . lift . lift . performEvent

instance (Reflex t, ReflexHost t, Ref m ~ Ref IO, MonadHold t m, PrimMonad (HostFrame t)) => Adjustable t (BasicGuest t m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace a0 a' =
    BasicGuest $ runWithReplace (unBasicGuest a0) (fmap unBasicGuest a')
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    BasicGuest $ traverseIntMapWithKeyWithAdjust (\k v -> unBasicGuest (f k v)) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    BasicGuest $ traverseDMapWithKeyWithAdjust (\k v -> unBasicGuest (f k v)) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    BasicGuest $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unBasicGuest (f k v)) dm0 dm'


instance (ReflexHost t) => NotReady t (BasicGuest t m) where
  {-# INLINABLE notReadyUntil #-}
  notReadyUntil _ = pure ()
  {-# INLINABLE notReady #-}
  notReady = pure ()

basicHostForever :: (forall t m. BasicGuestConstraints t m => BasicGuest t m a)
                 -> IO a
basicHostForever guest = basicHostWithQuit $ (\x -> (x, never)) <$> guest

basicHostWithQuit :: (forall t m. BasicGuestConstraints t m => BasicGuest t m (a, Event t ()))
                  -> IO a
basicHostWithQuit (BasicGuest guest) = do
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

repeatUntilQuit :: BasicGuestConstraints t m
                => Event t ()
                -> IO a
                -> BasicGuest t m ()
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

