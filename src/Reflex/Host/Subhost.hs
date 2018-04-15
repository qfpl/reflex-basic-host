{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Host.Subhost where

import Control.Monad (void, forever)
import Data.Functor.Identity (Identity(..))
import Data.Semigroup (Semigroup)

import Control.Monad.Trans (MonadIO(..))

import Data.GADT.Compare
import Data.Dependent.Sum
import qualified Data.Dependent.Map as DMap

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkIO)

import Reflex
import Reflex.Host.Basic

data SubhostInTag d e x where
  SIDynamic :: SubhostInTag d e d
  SIEvent :: SubhostInTag d e e

instance GEq (SubhostInTag d e) where
  geq SIDynamic SIDynamic = Just Refl
  geq SIEvent SIEvent = Just Refl
  geq _ _ = Nothing

instance GCompare (SubhostInTag d e) where
  gcompare SIDynamic SIDynamic = GEQ
  gcompare SIDynamic _ = GLT
  gcompare _ SIDynamic = GGT
  gcompare SIEvent SIEvent = GEQ

data SubhostOutTag c w x where
  SOOut :: SubhostOutTag c w c
  SOQuit :: SubhostOutTag c w ()
  SOWriter :: SubhostOutTag c w w

instance GEq (SubhostOutTag c w) where
  geq SOOut SOOut = Just Refl
  geq SOQuit SOQuit = Just Refl
  geq SOWriter SOWriter = Just Refl
  geq _ _ = Nothing

instance GCompare (SubhostOutTag c w) where
  gcompare SOOut SOOut = GEQ
  gcompare SOOut _ = GLT
  gcompare _ SOOut = GGT
  gcompare SOQuit SOQuit = GEQ
  gcompare SOQuit _ = GLT
  gcompare _ SOQuit = GGT
  gcompare SOWriter SOWriter = GEQ

subhost :: BasicGuestConstraints t m
        => Dynamic t a
        -> Event t b
        -> (forall t1 m1. BasicGuestConstraints t1 m1 => Dynamic t1 a -> Event t1 b -> BasicGuest t1 m1 (Event t1 c, Event t1 ()))
        -> BasicGuest t m (Event t c, Event t ())
subhost doA eoB fn = do
  (tAB, tCQ) <- liftIO . atomically $
    (,) <$> newEmptyTMVar <*> newEmptyTMVar

  (eoC, foC) <- newTriggerEvent
  (eoQ, foQ) <- newTriggerEvent

  void . liftIO . forkIO . forever $ do
    x <- atomically $ takeTMVar tCQ
    maybe (pure ()) (foC . runIdentity) (DMap.lookup SOOut x)
    maybe (pure ()) (foQ . runIdentity) (DMap.lookup SOQuit x)

  ioA <- sample . current $ doA
  let
    eAB = merge .
          DMap.fromList $ [ SIDynamic :=> updated doA
                          , SIEvent :=> eoB
                          ]
  performEvent_ $ liftIO . atomically . putTMVar tAB <$> eAB

  void . liftIO . forkIO $ basicHostWithQuit $ do
    (eiA, fiA) <- newTriggerEvent
    (eiB, fiB) <- newTriggerEvent

    void . liftIO . forkIO . forever $ do
      x <- atomically $ takeTMVar tAB
      maybe (pure ()) (fiA . runIdentity) (DMap.lookup SIDynamic x)
      maybe (pure ()) (fiB . runIdentity) (DMap.lookup SIEvent x)

    diA <- holdDyn ioA eiA
    (eiC, eQuit) <- fn diA eiB
    let
      eCQ = merge .
            DMap.fromList $ [ SOOut :=> eiC
                            , SOQuit :=> eQuit
                            ]
    performEvent_ $ liftIO . atomically . putTMVar tCQ <$> eCQ
    pure ((), eQuit)

  pure (eoC, eoQ)

subhostEventWriter :: (BasicGuestConstraints t m, Semigroup w)
                   => Dynamic t a
                   -> Event t b
                   -> (forall t1 m1. BasicGuestConstraints t1 m1 => Dynamic t1 a -> Event t1 b -> EventWriterT t1 w (BasicGuest t1 m1) (Event t1 c, Event t1 ()))
                   -> EventWriterT t w (BasicGuest t m) (Event t c, Event t ())
subhostEventWriter doA eoB fn = do
  (tAB, tCQW) <- liftIO . atomically $
    (,) <$> newEmptyTMVar <*> newEmptyTMVar

  (eoC, foC) <- newTriggerEvent
  (eoQ, foQ) <- newTriggerEvent
  (eoW, foW) <- newTriggerEvent

  void . liftIO . forkIO . forever $ do
    x <- atomically $ takeTMVar tCQW
    maybe (pure ()) (foC . runIdentity) (DMap.lookup SOOut x)
    maybe (pure ()) (foQ . runIdentity) (DMap.lookup SOQuit x)
    maybe (pure ()) (foW . runIdentity) (DMap.lookup SOWriter x)

  ioA <- sample . current $ doA
  let
    eAB = merge .
          DMap.fromList $ [ SIDynamic :=> updated doA
                          , SIEvent :=> eoB
                          ]
  performEvent_ $ liftIO . atomically . putTMVar tAB <$> eAB

  void . liftIO . forkIO $ basicHostWithQuit $ do
    (eiA, fiA) <- newTriggerEvent
    (eiB, fiB) <- newTriggerEvent

    void . liftIO . forkIO . forever $ do
      x <- atomically $ takeTMVar tAB
      maybe (pure ()) (fiA . runIdentity) (DMap.lookup SIDynamic x)
      maybe (pure ()) (fiB . runIdentity) (DMap.lookup SIEvent x)

    diA <- holdDyn ioA eiA
    ((eiC, eQuit), eiW) <- runEventWriterT $ fn diA eiB
    let
      eCQW = merge .
             DMap.fromList $ [ SOOut :=> eiC
                             , SOQuit :=> eQuit
                             , SOWriter :=> eiW
                             ]
    performEvent_ $ liftIO . atomically . putTMVar tCQW <$> eCQW
    pure ((), eQuit)

  tellEvent eoW

  pure (eoC, eoQ)

