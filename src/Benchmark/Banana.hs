{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-binds #-}
module Benchmark.Banana (
      benchmark1
    , benchmark2
    , main
    ) where

import           Benchmark.Utils
import           Control.Monad
import qualified Data.IntMap                as IM
import           Data.Time
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Mem
import           System.Random.MWC
import           Text.Printf


{-----------------------------------------------------------------------------
    Benchmarks
------------------------------------------------------------------------------}
benchmark1 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark1 netsize dur = do
    starttime <- getCurrentTime
    (addHandlers, triggers) <- unzip <$> replicateM netsize newAddHandler
    let
        trigMap = IM.fromList $ zip [0..netsize-1] triggers
        networkD :: forall t. Frameworks t => Moment t ()
        networkD = do
          evs <- mapM fromAddHandler addHandlers
          reactimate $ doSomething <$> unions evs

          {-
           - this implementation is slower (20-30%) than doing unions and a single
           - reactimate
          forM_ addHandlers $ \addHandler -> do
              ev <- fromAddHandler addHandler
              reactimate $ doSomething <$> ev
              -}
    network <- compile networkD
    actuate network
    midTime <- getCurrentTime

    randGen <- create
    forM_ [1..dur] $ \step -> do
      let str = show step
      replicateM_ 10 $ do
          ev <- uniformR (0,netsize-1) randGen
          maybe (return ()) ($ str) $ IM.lookup ev trigMap

    endTime <- getCurrentTime
    return (midTime `diffUTCTime` starttime, endTime `diffUTCTime` midTime)


benchmark2 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark2 netsize dur = do
  startTime <- getCurrentTime
  (unitEventHandlers, triggers) <- unzip <$> replicateM netsize newAddHandler
  (stepEventHandler, stepTrigger) <- newAddHandler
  let trigMap = IM.fromList $ zip [0..netsize-1] triggers
  let networkD :: forall t. Frameworks t => Moment t ()
      networkD = do
          unitEs <- mapM fromAddHandler unitEventHandlers
          stepE  <- fromAddHandler stepEventHandler

          let countBs = map count unitEs
          trimmedBs <- mapM trimB countBs

          let step10E = filterE (\cnt -> cnt `rem` 10 == 0) stepE
          let selectedB_E = head <$> accumE trimmedBs (keepTail <$ step10E)

          let selectedB = switchB (head countBs) selectedB_E
          let outputE = apply (const . doSomething . show <$> selectedB) stepE
          reactimate outputE

  network <- compile networkD
  actuate network

  midTime <- getCurrentTime
  randGen <- create

  forM_ [1..dur] $ \step -> do
      randomRs <- replicateM 10 $ uniformR (0,netsize-1) randGen

      stepTrigger step
      forM_ randomRs $ \ev -> maybe (error "banana bench2: trigger not found") ($ ()) $ IM.lookup ev trigMap
  endTime <- getCurrentTime
  return (midTime `diffUTCTime` startTime, endTime `diffUTCTime` midTime)

main :: IO ()
main = do
    let testN (lbl,bench) netsize dur = do
            putStrLn $ printf "%s iterations: %d netsize: %d" lbl dur netsize
            performGC
            (setup, run) <- bench netsize dur
            putStrLn $ printf "setup: %s\nruntime: %s" (show setup) (show run)
    let benches = [ ("benchmark 1", benchmark1)
                  , ("benchmark 2", benchmark2)
                  ]
        durs    = [100]
        sizes   = [100,1000]
    sequence_ $ testN <$> benches <*> sizes <*> durs

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
count :: Event t a -> Behavior t Int
count = accumB 0 . ((\(!n) -> n+1) <$)

keepTail :: [a] -> [a]
keepTail (_:y:zs) = y:zs
keepTail [x]      = [x]
keepTail []       = []
