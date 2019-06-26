{-# language FlexibleContexts, TypeFamilies #-}
module Main where

import Prelude hiding (filter)
import Data.Witherable (filter)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar, takeMVar)
import Control.Lens ((<&>))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>), void)
import Reflex
import Reflex.Host.Basic

left
  :: BasicGuestConstraints t m
  => MVar (String -> IO ())
  -> MVar (String -> IO ())
  -> BasicGuest t m (Event t (), ())
left mTriggerLeft mTriggerRight = do
  ePostBuild <- getPostBuild
  (eFromRight, triggerLeft) <- newTriggerEvent
  liftIO $ putMVar mTriggerLeft triggerLeft

  eTriggerRight <- performEvent $ ePostBuild $> liftIO (readMVar mTriggerRight)
  eLines <- performEventAsync $ eTriggerRight $> \fire ->
    void . liftIO . forkIO . forever $ getLine >>= fire

  bTriggerRight <- hold mempty eTriggerRight
  let firings = bTriggerRight <@> eLines
  performEvent_ $ liftIO <$> firings

  performEvent_ $ eFromRight <&> \msg ->
    liftIO . putStrLn $ "From Right: " <> msg

  pure (void $ filter (== "quit") eFromRight, ())

right
  :: BasicGuestConstraints t m
  => MVar (String -> IO ())
  -> MVar (String -> IO ())
  -> BasicGuest t m (Event t (), ())
right mTriggerLeft mTriggerRight = do
  ePostBuild <- getPostBuild
  (eFromLeft, triggerRight) <- newTriggerEvent
  liftIO $ putMVar mTriggerRight triggerRight

  eTriggerLeft <- performEvent $ ePostBuild $> liftIO (readMVar mTriggerLeft)
  bTriggerLeft <- hold mempty eTriggerLeft
  let firings = bTriggerLeft <@> eFromLeft
  performEvent_ $ liftIO <$> firings

  performEvent_ $ eFromLeft <&> \msg ->
    liftIO . putStrLn $ "From Left: " <> msg

  pure (void $ filter (== "quit") eFromLeft, ())

main :: IO ()
main = do
  mTriggerLeft <- newEmptyMVar :: IO (MVar (String -> IO ()))
  mTriggerRight <- newEmptyMVar :: IO (MVar (String -> IO ()))

  mLeftDone <- newEmptyMVar
  mRightDone <- newEmptyMVar

  void . forkIO $ do
    () <- basicHostWithQuit (left mTriggerLeft mTriggerRight)
    putMVar mLeftDone ()

  void . forkIO $ do
    () <- basicHostWithQuit (right mTriggerLeft mTriggerRight)
    putMVar mRightDone ()

  -- Wait for both threads
  takeMVar mLeftDone
  takeMVar mRightDone
