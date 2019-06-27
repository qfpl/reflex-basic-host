{-# language FlexibleContexts, TypeFamilies #-}
module Main where

{- |

* Multithreaded Multi-Host Example

This example boots two reflex networks which we will call "Left" and
"Right". As they start, they each write an event trigger into an
'MVar' and read an event trigger from another 'MVar', so that they can
fire events into each other's network.

Left and Right both log to stdout any lines received from each
other. Left also reads lines from stdin, and lines are passed around
as follows: Left passes incoming lines to Right, which immediately
sends received lines back to Left.

If "quit" is read from stdin, this triggers a shutdown of both FRP
networks.

-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar, takeMVar)
import Control.Lens ((<&>))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>), void)
import Data.Semigroup ((<>))
import Data.Witherable (filter)
import Prelude hiding (filter)
import Reflex
import Reflex.Host.Basic

left
  :: BasicGuestConstraints t m
  => MVar (String -> IO ())
  -> MVar (String -> IO ())
  -> BasicGuest t m (Event t ())
left mTriggerLeft mTriggerRight = do
  -- Construct the event "lines from Right", and put its trigger in an MVar.
  (eFromRight, triggerLeft) <- newTriggerEvent
  liftIO $ putMVar mTriggerLeft triggerLeft

  -- Kick off a loop to read from stdin. Ignore exceptions for brevity.
  ePostBuild <- getPostBuild
  eLines <- performEventAsync $ ePostBuild $> \fire ->
    void . liftIO . forkIO . forever $ getLine >>= fire

  -- Get the event trigger for the "lines from Left" event inside the
  -- Right network, and fire it on each line.
  triggerRight <- liftIO $ readMVar mTriggerRight
  performEvent_ $ liftIO . triggerRight <$> eLines

  -- Log events received from Right.
  performEvent_ $ eFromRight <&> \msg ->
    liftIO . putStrLn $ "From Right: " <> msg

  -- Quit if we get a "quit" from Right.
  pure . void $ filter (== "quit") eFromRight

right
  :: BasicGuestConstraints t m
  => MVar (String -> IO ())
  -> MVar (String -> IO ())
  -> BasicGuest t m (Event t ())
right mTriggerLeft mTriggerRight = do
  -- Construct the event "lines from Left", and put its trigger in an MVar.
  (eFromLeft, triggerRight) <- newTriggerEvent
  liftIO $ putMVar mTriggerRight triggerRight

  -- Get the event trigger for the "lines from Left" event inside the
  -- Right network, and fire it on each line.
  triggerLeft <- liftIO $ readMVar mTriggerLeft
  performEvent_ $ liftIO . triggerLeft <$> eFromLeft

  -- Log events received from Left.
  performEvent_ $ eFromLeft <&> \msg ->
    liftIO . putStrLn $ "From Left: " <> msg

  -- Quit if we get a "quit" from Left.
  pure . void $ filter (== "quit") eFromLeft

main :: IO ()
main = do
  -- Removing these type annotations causes type errors like "a0 is
  -- untouchable".
  mTriggerLeft <- newEmptyMVar :: IO (MVar (String -> IO ()))
  mTriggerRight <- newEmptyMVar :: IO (MVar (String -> IO ()))

  mLeftDone <- newEmptyMVar
  mRightDone <- newEmptyMVar

  -- Not exception-safe, for brevity's sake.
  void . forkIO $ do
    basicHostWithQuit (left mTriggerLeft mTriggerRight)
    putMVar mLeftDone ()

  void . forkIO $ do
    basicHostWithQuit (right mTriggerLeft mTriggerRight)
    putMVar mRightDone ()

  -- Wait for both threads
  takeMVar mLeftDone
  takeMVar mRightDone
