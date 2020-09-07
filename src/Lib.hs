module Lib
  ( update
  , avg
  )
where

import qualified Customer                      as C
import           Customer                       ( Customer )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Dequeue                  as Q
import           TimeInSeconds                  ( TimeInSeconds )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

update
  :: (Q.Dequeue q, Num a, Ord a)
  => ([Customer], q Customer, TimeInSeconds, [TimeInSeconds], Set a)
  -> TimeInSeconds
  -> ([Customer], q Customer, TimeInSeconds, [TimeInSeconds], Set a)
update (customers, queue, procTime, waitingTimes, qSizes) curTime =
  (customers', queue', procTime', waitingTimes', qSizes')
 where
  (maybeCustomer, customers') = case customers of
    (c : cs) | C.arrivalTime c == curTime -> (Just c, cs)
    _ -> (Nothing, customers)
  nProcTime        = procTime - 1
  isTillBusy       = nProcTime > 0
  tempQueue        = maybe queue (Q.pushBack queue) maybeCustomer
  maybeElAndQ      = Q.popFront tempQueue
  maybeNewProcTime = C.processingTime . fst <$> maybeElAndQ
  maybeArrivalTime = C.arrivalTime . fst <$> maybeElAndQ
  maybeQ           = snd <$> maybeElAndQ
  queue'           = if isTillBusy then tempQueue else fromMaybe Q.empty maybeQ
  lengthQueue'     = fromIntegral $ length queue'
  qSizes'          = Set.insert lengthQueue' qSizes
  procTime' = if isTillBusy then nProcTime else fromMaybe 0 maybeNewProcTime
  waitingTimes'    = if isTillBusy
    then waitingTimes
    else maybe waitingTimes
               (\at -> (curTime - at) : waitingTimes)
               maybeArrivalTime

avg :: (Fractional a) => [a] -> Maybe a
avg [] = Nothing
avg as = Just $ sum / numEls
 where
  (sum, numEls) =
    foldr (\el (sum', numEls') -> (sum' + el, numEls' + 1)) (0, 0) as
