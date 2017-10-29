{-# LANGUAGE Arrows          #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- base
import Control.Concurrent -- (threadDelay)
import Data.Either (rights)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Monad (void)

-- dunai
import Control.Monad.Trans.MSF.Maybe (runMaybeT, MaybeT, exit)

import UI.NCurses

import System.Random

import Text.Printf

import CursesClock

-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.Clock.Select
import FRP.Rhine.Schedule.Concurrently
import FRP.Rhine.Schedule.Trans
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.ResamplingBuffer.Collect


data Stock =
  Stock {
  stock :: String
  , value :: Double
  }

type StockClock n m = HoistClock IO m (Millisecond n)

monadIOClock :: (MonadIO m) => cl -> HoistClock IO m cl
monadIOClock cl = HoistClock cl liftIO


myClock0 :: (MonadIO m) => StockClock 1200 m
myClock0 = monadIOClock waitClock

myClock1 :: (MonadIO m) => StockClock 1000 m
myClock1 = monadIOClock waitClock

myClock2 :: (MonadIO m) => StockClock 1600 m
myClock2 = monadIOClock waitClock

type EventClock = SelectClock CursesClock Event

eventClock :: EventClock
eventClock = SelectClock CursesClock id

stockLenses :: Stock
stockLenses = Stock "lenses" 2.7

stockRB :: Stock
stockRB = Stock "reactive-banana" 3.2

stockMedModule :: Stock
stockMedModule = Stock "med-module" 2.4

initialiseDisplay :: Curses ()
initialiseDisplay = do
  w <- defaultWindow
  setCursorMode CursorInvisible
  updateWindow w $ do 
    drawBox Nothing  Nothing
    moveCursor 0 4
    drawString " Welcome to the Haskell Stock Exchange "
  render
  
displayStock :: Integer -> Integer -> Stock -> Curses ()
displayStock x y Stock{..} = do
  w <- defaultWindow
  updateWindow w $ do
    moveCursor y x
    drawString (printf "%24s" (stock ++ ": "))
    setAttribute AttributeBold True
    drawString (printf "%.2f" value)
    setAttribute AttributeBold False
  render

displayEvent :: String -> Curses ()
displayEvent str = do
  w <- defaultWindow
  updateWindow w $ do
    moveCursor 15 10
    drawString str
  render
  
modifyStock :: Integer -> Stock -> SyncSF (ExceptT () Curses) (StockClock n m) () ()
modifyStock i Stock{..} = proc () -> do
  v <- arrMSync (\x -> fmap (+x) (liftIO (randomRIO (-1, 1)))) -< value 
  _ <- arrMSync (\v -> lift (displayStock 4 (10+i) (Stock stock v))) -< v
  returnA -< ()

syncExcept :: Integer -> Stock -> SyncExcept Curses (StockClock n m) () () Empty
syncExcept i stock = do
  try (modifyStock i stock)
  syncExcept i stock

event :: SyncSF Curses (HoistClock m Curses EventClock) () ()
event = proc () -> do
  evt <- timeInfoOf tag -< ()
  case evt of
    EventCharacter x -> do
      _ <- arrMSync displayEvent -< show evt
      returnA -< ()
    _ -> returnA -< ()
      
  

conc :: (TimeDomainOf cl1 ~ TimeDomainOf cl2, Clock IO cl1, Clock IO cl2) => Schedule Curses (HoistClock IO Curses cl1) (HoistClock IO Curses cl2)
conc = hoistClockSchedule liftIO concurrently

conc2 :: (TimeDomainOf cl1 ~ TimeDomainOf cl2, MonadIO m, Clock IO cl1, Clock IO cl2) => Schedule m cl1 cl2
conc2 = hoistSchedule liftIO concurrently

hcl :: (MonadIO m) => HoistClock IO m EventClock
hcl = HoistClock {
  hoistedClock = eventClock
  , monadMorphism = liftIO
  }

main :: IO ()
main = do
  let a = safely (syncExcept 0 stockLenses) @@ (myClock0 :: StockClock 1200 Curses)
      b = safely (syncExcept 1 stockRB) @@ (myClock1 :: StockClock 1000 Curses)
      c = safely (syncExcept 2 stockMedModule) @@ (myClock2 :: StockClock 1600 Curses)

      input = event @@ hcl
      
  runCurses $ do
    initialiseDisplay
    flow (a **@ conc @** b)

    -- Unfortunatly, this does not work:
    -- flow ((a **@ conc @** b) **@ conc @** c)

    -- Here is a simple type error which I do not know how to fix!
    -- flow input

    -- Ideally this should work:
    -- flow (((a **@ conc @** b) **@ conc @** c) **@ conc @** input)

