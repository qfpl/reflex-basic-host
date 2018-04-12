{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main (
    main
  ) where

import Control.Monad (void, forever)
import Text.Read (readMaybe)

import Control.Concurrent (forkIO)

import Control.Monad.Trans (MonadIO(..))

import qualified Data.Map as Map

import Data.Time.Clock

import Reflex
import Reflex.Host.Basic
import Reflex.Host.Subhost

parseAdd :: String -> Maybe (Int, String)
parseAdd s =
  case words s of
    ("add" : i : xs) -> case readMaybe i of
      Just i' -> pure (i', unwords xs)
      Nothing -> Nothing
    _ -> Nothing

parseDel :: String -> Maybe Int
parseDel s =
  case words s of
    ["del", i] -> case readMaybe i of
      Just i' -> pure i'
      Nothing -> Nothing
    _ ->  Nothing

main :: IO ()
main = basicHostWithQuit $ mdo
  (eLine, fireLine) <- newTriggerEvent
  void . liftIO . forkIO . forever $
    fireLine =<< getLine

  let
    eAdd = fmapMaybe parseAdd eLine
    eDel = fmapMaybe parseDel eLine

  eIns <- numberOccurrences eAdd
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
            uncurry Map.insert <$> eIns
          , flip (foldr Map.delete) <$> eRemoves
          ]

  performEvent_ $ liftIO . putStrLn . ("Added with id: " ++) . show . fst <$> eIns

  dme <- listWithKey dMap $ \k dv -> do
    (_ :: Event t (), eQuitSub) <- subhost dv (void . ffilter (== k) $ eDel) $ \dv' eQuit' -> do
      (tick, line) <- sample . current $ dv'

      start <- liftIO getCurrentTime
      eTick <- tickLossy (fromIntegral tick) start
      performEvent_ $ liftIO (putStrLn line) <$ eTick

      pure (never, eQuit')

    pure eQuitSub

  let
    eRemoves = fmap Map.keys . switchDyn . fmap mergeMap $ dme

  dQuit <- holdDyn False . updated $ (== 0) . Map.size <$> dMap
  let eQuit = void . ffilter id . updated $ dQuit

  pure ((), eQuit)
