module Main where

import           Customer                       ( Customer
                                                , ProcessingTimeParams
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
  yellowCustomers <- customers yellow
  let (_, _, _, yellowWaitingTimes', _) = foldl'
        update
        (initialState yellowCustomers)
        (timeInterval yellowCustomers)
  let yAvg = fromMaybe noAvg . avg $ fromIntegral <$> yellowWaitingTimes'
  let yMax   = maximum yellowWaitingTimes'
  let yDelta = fromIntegral yMax - yAvg
  print $ "- average customer waiting time: " <> show yAvg
  print $ "- maximum customer waiting time: " <> show yMax

  putStrLn ""

  print
    "Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
  redCustomers <- customers red
  let (_, _, _, redWaitingTimes', qSizes') =
        foldl' update (initialState redCustomers) (timeInterval redCustomers)
  let rAvg = fromMaybe noAvg . avg $ fromIntegral <$> redWaitingTimes'
  let rMax   = maximum redWaitingTimes'
  let rDelta = fromIntegral rMax - rAvg
  print $ "- average queue length: " <> (show . fromMaybe 0.0 . avg)
    (fromIntegral <$> Set.toList qSizes')
  print $ "- maximum queue length: " <> show (maximum qSizes')

  putStrLn ""

  print
    "Which type of customer (yellow, red or blue) gives the closest value between the average and maximum customer waiting times?"
  blueCustomers <- customers blue
  let (_, _, _, blueWaitingTimes', _) =
        foldl' update (initialState blueCustomers) (timeInterval blueCustomers)
  let bAvg = fromMaybe noAvg . avg $ fromIntegral <$> blueWaitingTimes'
  let bMax   = maximum blueWaitingTimes'
  let bDelta = fromIntegral bMax - bAvg
  let closest = if yDelta == rDelta && rDelta == bDelta
        then "No difference"
        else case minimum [yDelta, rDelta, bDelta] of
          x | x == yDelta -> "Yellow"
          x | x == rDelta -> "Red"
          x | x == bDelta -> "Blue"
          _               -> "Unexpected result"
  print closest

noAvg :: (Fractional a) => a
noAvg = 0.0

customersLimit :: Int
customersLimit = 500

initialState
  :: (Num a, Ord a)
  => [Customer]
  -> ( [Customer]
     , Q.BankersDequeue Customer
     , TimeInSeconds
     , [TimeInSeconds]
     , Set a
     )
initialState customers = (customers, Q.empty, 0, [], Set.empty)

customers :: ProcessingTimeParams -> IO [Customer]
customers params = do
  customersStream <- mkCustomers params
  return $ take customersLimit customersStream

timeInterval :: [Customer] -> [TimeInSeconds]
timeInterval customers =
  let maxTime = arrivalTime (last customers) in [0 .. maxTime]
