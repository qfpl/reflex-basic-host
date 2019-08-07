{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo      #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Reflex
import Reflex.Host.Basic

myNetwork
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Event t ()
  -> m (Dynamic t Int)
myNetwork = count

myGuest :: BasicGuestConstraints t m => BasicGuest t m (Event t ())
myGuest = mdo
  eTick <- repeatUntilQuit (void $ threadDelay 1000000) eQuit
  let
    eCountUpdated = updated dCount
    eQuit = () <$ ffilter (==5) eCountUpdated
  dCount <- myNetwork eTick

  performEvent_ $ liftIO . print <$> eCountUpdated
  pure eQuit

main :: IO ()
main = basicHostWithQuit myGuest
