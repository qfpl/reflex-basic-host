{-# LANGUAGE RecursiveDo #-}
module Main (
    main
  ) where

import Control.Monad (void)
import Control.Monad.Trans (liftIO)

import Reflex

import Reflex.Host.Basic

main :: IO ()
main = basicHostWithQuit $ mdo
  -- ePostBuild <- getPostBuild
  -- eLine <- performEventAsync $ (\fn -> liftIO $ fn =<< getLine) <$ leftmost [ePostBuild, void eMessage]
  (eLine, onLine) <- newTriggerEvent
  repeatUntilQuit eQuit (onLine =<< getLine)

  let
    eMessage = ffilter (/= "quit") eLine
    eQuit = void . ffilter (== "quit") $ eLine

  performEvent_ $ liftIO . putStrLn <$> eMessage

  pure ((), eQuit)
