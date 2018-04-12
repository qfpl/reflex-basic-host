{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Host.Subhost where

import Control.Monad (void, forever)

import Control.Monad.Trans (MonadIO(..))

import Data.Align
import Data.These

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkIO)

import Reflex
import Reflex.Host.Basic

subhost :: Dynamic t a
        -> Event t b
        -> (forall t1 m1. Dynamic t1 a -> Event t1 b -> BasicGuest t1 m1 (Event t1 c, Event t1 ()))
        -> BasicGuest t m (Event t c, Event t ())
subhost doA eoB fn = do
  (tAB, tCQ) <- liftIO . atomically $
    (,) <$> newEmptyTMVar <*> newEmptyTMVar

  (eoC, foC) <- newTriggerEvent
  (eoQ, foQ) <- newTriggerEvent

  void . liftIO . forkIO . forever $ do
    x <- atomically $ takeTMVar tCQ
    case x of
      This c -> foC c
      That q -> foQ q
      These c q -> foC c >> foQ q

  ioA <- sample . current $ doA
  let
    eAB = align (updated doA) eoB
  performEvent_ $ liftIO . atomically . putTMVar tAB <$> eAB

  void . liftIO . forkIO $ basicHostWithQuit $ do
    (eiA, fiA) <- newTriggerEvent
    (eiB, fiB) <- newTriggerEvent

    void . liftIO . forkIO . forever $ do
      x <- atomically $ takeTMVar tAB
      case x of
        This a -> fiA a
        That b -> fiB b
        These a b -> fiA a >> fiB b

    diA <- holdDyn ioA eiA
    (eiC, eQuit) <- fn diA eiB
    let
      eCQ = align eiC eQuit
    performEvent_ $ liftIO . atomically . putTMVar tCQ <$> eCQ
    pure ((), eQuit)

  pure (eoC, eoQ)

