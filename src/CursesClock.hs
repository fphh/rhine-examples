{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CursesClock where

-- base
import Data.Time.Clock

-- transformers
import Control.Monad.IO.Class

-- rhine
import FRP.Rhine

import UI.NCurses

{- |
A clock that ticks for every Curses event.
-}
data CursesClock = CursesClock

instance Clock Curses CursesClock where
  type TimeDomainOf CursesClock = UTCTime
  type Tag          CursesClock = Maybe Event

  startClock _ = do

    let getEv = do
          w <- defaultWindow
          getEvent w Nothing
                    
    initialTime <- liftIO getCurrentTime
    return
      (     arrM_ (liftIO getCurrentTime)
        &&& arrM_ getEv
      , initialTime
      )


instance Monoid CursesClock where
  mempty      = CursesClock
  mappend _ _ = CursesClock
