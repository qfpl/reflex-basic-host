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
  ) => TriggerEventT t (PostBuildT t (PerformEventT t m)) a

basicHost :: (forall t m. BasicGuest t m a) -> IO a
basicHost guest = do
  events <- newChan
  (res, fc) <- runSpiderHost $ do
    (eOpen, eOpenRef) <- newEventWithTriggerRef

    (res, fc) <- hostPerformEventT . flip runPostBuildT eOpen . flip runTriggerEventT events $ guest

    mTriggerO <- liftIO . readIORef $ eOpenRef
    forM_ mTriggerO $ \t ->
      runFireCommand fc [t :=> Identity ()] (return ())

    return (res, fc)

  -- this comes directly from reflex-dom, could potentially be brought across into reflex (probably in TriggerEvent.Base)
  void . forkIO . forever $ do
    ers <- readChan events
    runSpiderHost $ do
      mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
        me <- readIORef er
        return $ fmap (\e -> e :=> Identity a) me
      _ <- runFireCommand fc (catMaybes mes) $ return ()
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb

  return res
