module Main where

import           Customer                       ( Customer
                                                , mkCustomers
                                                , arrivalTime
                                                , yellow
                                                , red
                                                , blue
                                                )
import           TimeInSeconds                  ( TimeInSeconds )
import           Lib                            ( update
                                                , avg
                                                )
import qualified Data.Dequeue                  as Q
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromMaybe )

main :: IO ()
main = do
  let noAvg :: (Fractional a) => a
      noAvg = 0.0
  print
    "Given only yellow customers, what are the average and maximum customer waiting times?"
  yellowCustomersStream <- mkCustomers yellow
  let yellowCustomers = take 50 yellowCustomersStream
  let maxTimeYellow   = arrivalTime $ last yellowCustomers
  let yTs             = [0 .. maxTimeYellow]
  let initialState
        :: (Num a, Ord a)
        => ( [Customer]
           , Q.BankersDequeue Customer
           , TimeInSeconds
           , [TimeInSeconds]
           , Set a
           )
      initialState = (yellowCustomers, Q.empty, 0, [], Set.empty)
  let (_, _, _, yellowWaitingTimes', _) = foldl' update initialState yTs
  let yAvg = fromMaybe noAvg . avg $ fromIntegral <$> yellowWaitingTimes'
  let yMax                              = maximum yellowWaitingTimes'
  let yDelta                            = fromIntegral yMax - yAvg
  print $ "- average customer waiting time: " <> show yAvg
  print $ "- maximum customer waiting time: " <> show yMax

  putStrLn ""

  print
    "Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
  redCustomersStream <- mkCustomers red
  let redCustomers = take 50 redCustomersStream
  let maxTimeRed   = arrivalTime $ last redCustomers
  let rTs          = [0 .. maxTimeRed]
  let initialState
        :: (Num a, Ord a)
        => ( [Customer]
           , Q.BankersDequeue Customer
           , TimeInSeconds
           , [TimeInSeconds]
           , Set a
           )
      initialState = (redCustomers, Q.empty, 0, [], Set.empty)
  let (_, _, _, redWaitingTimes', qSizes') = foldl' update initialState rTs
  let rAvg = fromMaybe noAvg . avg $ fromIntegral <$> redWaitingTimes'
  let rMax   = maximum redWaitingTimes'
  let rDelta = fromIntegral rMax - rAvg
  print $ "- average queue length: " <> (show . fromMaybe 0.0 . avg)
    (fromIntegral <$> Set.toList qSizes')
  print $ "- maximum queue length: " <> show (maximum qSizes')

  putStrLn ""

  print
    "Which type of customer (yellow, red or blue) gives the closest value between the average and maximum customer waiting times?"
  blueCustomersStream <- mkCustomers blue
  let blueCustomers = take 50 blueCustomersStream
  let maxTimeBlue   = arrivalTime $ last blueCustomers
  let bTs           = [0 .. maxTimeBlue]
  let initialState
        :: (Num a, Ord a)
        => ( [Customer]
           , Q.BankersDequeue Customer
           , TimeInSeconds
           , [TimeInSeconds]
           , Set a
           )
      initialState = (blueCustomers, Q.empty, 0, [], Set.empty)
  let (_, _, _, blueWaitingTimes', _) = foldl' update initialState bTs
  let bAvg = fromMaybe noAvg . avg $ fromIntegral <$> blueWaitingTimes'
  let bMax                            = maximum blueWaitingTimes'
  let bDelta                          = fromIntegral bMax - bAvg
  let closest = if yDelta == rDelta && rDelta == bDelta
        then "No difference"
        else case minimum [yDelta, rDelta, bDelta] of
          x | x == yDelta -> "Yellow"
          x | x == rDelta -> "Red"
          x | x == bDelta -> "Blue"
          _               -> "Unexpected result"
  print closest
