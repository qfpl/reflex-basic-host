{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>), void)
import Reflex
import Reflex.Host.Basic (basicHostWithQuit)

main :: IO ()
main = basicHostWithQuit $ mdo
  ePostBuild <- getPostBuild
  eLine <- performEventAsync $ leftmost [void eMessage, ePostBuild] $> \fn ->
    liftIO $ fn =<< getLine

  let
    eMessage = ffilter (/= "quit") eLine
    eQuit = void . ffilter (== "quit") $ eLine

  performEvent_ $ liftIO . putStrLn <$> eMessage

  pure eQuit
