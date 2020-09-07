module Customer
  ( Customer(..)
  , ProcessingTimeParams
  , mkCustomers
  , yellow
  , red
  , blue
  )
where

import           System.Random                  ( getStdGen
                                                , Random(randomRs)
                                                )

import           Data.Maybe                     ( maybeToList )

import           GHC.Float.RealFracMethods      ( ceilingFloatInteger )

import           TimeInSeconds                  ( TimeInSeconds )

import           Control.Applicative            ( ZipList(ZipList, getZipList) )

data ProcessingTimeParams = ProcessingTimeParams { getAlpha :: Int, getBeta :: Int}

yellow :: ProcessingTimeParams
yellow = ProcessingTimeParams 2 5

red :: ProcessingTimeParams
red = ProcessingTimeParams 2 2

blue :: ProcessingTimeParams
blue = ProcessingTimeParams 5 1

data Customer = Customer { arrivalTime :: TimeInSeconds, processingTime :: TimeInSeconds } deriving (Eq, Show)

mkCustomers :: ProcessingTimeParams -> IO [Customer]
mkCustomers params = do
  aas <- actualArrivals
  pts <- processingTimes params
  let customers = Customer <$> ZipList aas <*> ZipList pts
  return $ getZipList customers

scanSum :: Num a => [a] -> [a]
scanSum = scanl1 (+)

zerosToMaxs :: (Random a, Num a, Bounded a) => IO [a]
zerosToMaxs = randomRs (0, 3600) <$> getStdGen

zerosToOnes :: (Random a, Fractional a) => IO [a]
zerosToOnes = randomRs (0.0, 1.0) <$> getStdGen

arrivalTimeProb :: (Floating a1, Integral a2) => a2 -> a1
arrivalTimeProb t =
  let a = 100 in 1 - exp (negate $ fromIntegral t / fromIntegral a)

maybeArrivalTime :: Integral a => a -> Maybe a
maybeArrivalTime t = if arrivalTimeProb t >= 0.5 then Just t else Nothing

processingTime' :: (Integral b1, Integral b2, Num a) => b1 -> b2 -> a -> a
processingTime' a b x = let p = 200 in p * x ^ (a - 1) * (1 - x) ^ (b - 1)

actualArrivals :: IO [TimeInSeconds]
actualArrivals = do
  d <- zerosToMaxs :: IO [Int]
  let ints           = toInteger <$> d
  let actualArrivals = ints >>= (maybeToList . maybeArrivalTime)
  let inTime         = scanSum actualArrivals
  return inTime


processingTimes :: ProcessingTimeParams -> IO [TimeInSeconds]
processingTimes params = do
  ztos <- zerosToOnes :: IO [Float]
  let processingTimeColour = processingTime' (getAlpha params) (getBeta params)
  return $ ceilingFloatInteger . processingTimeColour <$> ztos
