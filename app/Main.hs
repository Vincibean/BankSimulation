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
  print
    "Given only yellow customers, what are the average and maximum customer waiting times?"
  yellowCustomersStream <- mkCustomers yellow
  let yellowCustomers = take 50 yellowCustomersStream
  let maxTime         = arrivalTime $ last yellowCustomers
  let ts              = [0 .. maxTime]
  let initialState
        :: (Num a, Ord a)
        => ( [Customer]
           , Q.BankersDequeue Customer
           , TimeInSeconds
           , [TimeInSeconds]
           , Set a
           )
      initialState = (yellowCustomers, Q.empty, 0, [], Set.empty)
  let (_, _, _, waitingTimes', _) = foldl' update initialState ts
  print $ "- average customer waiting time: " <> (show . fromMaybe 0.0 . avg)
    (fromIntegral <$> waitingTimes')
  print $ "- maximum customer waiting time: " <> show (maximum waitingTimes')

  putStrLn ""

  print
    "Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
  redCustomersStream <- mkCustomers red
  let redCustomers = take 50 redCustomersStream
  let maxTime      = arrivalTime $ last redCustomers
  let ts           = [0 .. maxTime]
  let initialState
        :: (Num a, Ord a)
        => ( [Customer]
           , Q.BankersDequeue Customer
           , TimeInSeconds
           , [TimeInSeconds]
           , Set a
           )
      initialState = (redCustomers, Q.empty, 0, [], Set.empty)
  let (_, _, _, _, qSizes') = foldl' update initialState ts
  print $ "- average queue length: " <> (show . fromMaybe 0.0 . avg)
    (fromIntegral <$> Set.toList qSizes')
  print $ "- maximum queue length: " <> show (maximum qSizes')
