module LibSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Lib
import           Customer
import qualified Data.Dequeue                  as Q
import           TimeInSeconds                  ( TimeInSeconds )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

instance Arbitrary Customer where
  arbitrary =
    Customer <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)

initialCustomerList :: [Customer]
initialCustomerList = []

initialCustomerQueue :: Q.BankersDequeue Customer
initialCustomerQueue = Q.empty

initialProcessingTime :: TimeInSeconds
initialProcessingTime = 0

initialWaitingTimes :: [TimeInSeconds]
initialWaitingTimes = []

initialQueueSizes :: (Num a, Ord a) => Set a
initialQueueSizes = Set.empty

initialState
  :: (Num a, Ord a)
  => ( [Customer]
     , Q.BankersDequeue Customer
     , TimeInSeconds
     , [TimeInSeconds]
     , Set a
     )
initialState =
  ( initialCustomerList
  , initialCustomerQueue
  , initialProcessingTime
  , initialWaitingTimes
  , initialQueueSizes
  )

currentTime :: TimeInSeconds
currentTime = 1

c1 = Customer 10 2

c2 = Customer 9 3

nonEmptyQueue :: Q.BankersDequeue Customer
nonEmptyQueue = Q.pushBack Q.empty c1

spec :: Spec
spec = do
  describe "The update function" $ do
    it
        "should use an empty list of customers if an empty list of customers is given"
      $ do
          let (customers', _, _, _, _) = update initialState currentTime
          customers' `shouldBe` []
    it
        "should dequeue a customer from the list of customers if a non-empty list of customers is given and the arrival time is right"
      $ do
          let customerList = [c1, c2]
          let (customers', _, _, _, _) = update
                ( customerList
                , initialCustomerQueue
                , initialProcessingTime
                , initialWaitingTimes
                , initialQueueSizes
                )
                (arrivalTime c1)
          customers' `shouldBe` [c2]
    it
        "should give back the empty queue if an empty queue is given and the list of customers is empty"
      $ do
          let (_, queue', _, _, _) = update initialState currentTime
          queue' `shouldBe` initialCustomerQueue
    it
        "should give back the original queue if an empty list of customers is given"
      $ do
          let (_, queue', _, _, _) = update
                ( initialCustomerList
                , nonEmptyQueue
                , initialProcessingTime
                , initialWaitingTimes
                , initialQueueSizes
                )
                currentTime
          queue' `shouldBe` initialCustomerQueue
    it
        "should dequeue a customer if a non-empty list of customers and queue of customers are given and the till isn't busy"
      $ do
          let updatedQueue :: Q.BankersDequeue Customer
              updatedQueue = Q.pushBack Q.empty c2
          let customerList = [c2]
          let (_, queue', _, _, _) = update
                ( customerList
                , nonEmptyQueue
                , initialProcessingTime
                , initialWaitingTimes
                , initialQueueSizes
                )
                (arrivalTime c2)
          queue' `shouldBe` updatedQueue
    it
        "should enqueue a customer if a non-empty list of customers is given and the till is busy"
      $ do
          let customerList = [c1]
          let (_, queue', _, _, _) = update
                ( customerList
                , initialCustomerQueue
                , 42
                , initialWaitingTimes
                , initialQueueSizes
                )
                (arrivalTime c1)
          queue' `shouldBe` nonEmptyQueue
    it
        "should give back the original queue if an empty list of customers is given and the till is busy"
      $ do
          let (_, queue', _, _, _) = update
                ( initialCustomerList
                , initialCustomerQueue
                , 42
                , initialWaitingTimes
                , initialQueueSizes
                )
                currentTime
          queue' `shouldBe` initialCustomerQueue
    it "should decrement the processing time by 1 if the till is busy" $ do
      let procTime = 42
      let (_, _, procTime', _, _) = update
            ( initialCustomerList
            , initialCustomerQueue
            , procTime
            , initialWaitingTimes
            , initialQueueSizes
            )
            currentTime
      procTime' `shouldBe` procTime - 1
    it
        "should update the processing time to the processing time of the next customer in the queue if the till is not busy"
      $ do
          let customerList = [c2]
          let (_, _, procTime', _, _) = update
                ( customerList
                , nonEmptyQueue
                , initialProcessingTime
                , initialWaitingTimes
                , initialQueueSizes
                )
                currentTime
          procTime' `shouldBe` processingTime c1
    it
        "should update the processing time to 0 if the till is not busy and the customer queue is empty and the list of customers is empty"
      $ do
          let (_, _, procTime', _, _) = update
                ( initialCustomerList
                , initialCustomerQueue
                , initialProcessingTime
                , initialWaitingTimes
                , initialQueueSizes
                )
                currentTime
          procTime' `shouldBe` 0
    it
        "should add a new waiting time to the list as soon as the till is not busy anymore"
      $ do
          let (_, _, _, waitingTimes, _) = update
                ( initialCustomerList
                , nonEmptyQueue
                , initialProcessingTime
                , initialWaitingTimes
                , initialQueueSizes
                )
                currentTime
          head waitingTimes `shouldBe` currentTime - arrivalTime c1
    it "should not add a new waiting time to the list if the till is busy" $ do
      let (_, _, _, waitingTimes, _) = update
            ( initialCustomerList
            , nonEmptyQueue
            , 42
            , initialWaitingTimes
            , initialQueueSizes
            )
            currentTime
      waitingTimes `shouldBe` initialWaitingTimes
    it
        "should not add a new waiting time to the list if the till is not busy but the queue is empty"
      $ do
          let (_, _, _, waitingTimes, _) = update
                ( initialCustomerList
                , initialCustomerQueue
                , initialProcessingTime
                , initialWaitingTimes
                , initialQueueSizes
                )
                currentTime
          waitingTimes `shouldBe` initialWaitingTimes
    it "should insert a new queue size if the current queue size is new" $ do
      let queueSizes   = Set.fromList [5, 6, 7]
      let customerList = [c1]
      let (_, _, _, _, queueSizes') = update
            ( customerList
            , initialCustomerQueue
            , 42
            , initialWaitingTimes
            , queueSizes
            )
            (arrivalTime c1)
      Set.member 1 queueSizes' `shouldBe` True
  describe "The avg function" $ do
    it "should give back the average if the list is not empty"
      $          avg [1, 2, 3]
      `shouldBe` Just 2
    it "should give nothing back if the list is empty"
      $          avg []
      `shouldBe` Nothing
