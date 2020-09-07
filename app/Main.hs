module Main where

import           Customer                       ( Customer
                                                , mkCustomers
                                                , arrivalTime
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
  print
    "Given only yellow customers, what are the average and maximum customer waiting times?"
  customersStream <- mkCustomers
  let customers = take 50 customersStream
  let maxTime   = arrivalTime $ last customers
  let ts        = [0 .. maxTime]
  let initialState
        :: (Num a, Ord a)
        => ( [Customer]
           , Q.BankersDequeue Customer
           , TimeInSeconds
           , [TimeInSeconds]
           , Set a
           )
      initialState = (customers, Q.empty, 0, [], Set.empty)
  let (customers', queue', procTime', waitingTimes', qSizes') =
        foldl' update initialState ts
  print $ "- average customer waiting time: " <> (show . fromMaybe 0.0 . avg)
    (fromIntegral <$> waitingTimes')
  print $ "- maximum customer waiting time: " <> show (maximum waitingTimes')
