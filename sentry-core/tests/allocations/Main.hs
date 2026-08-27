-- | Ensure that our scope mechanism for accumulating metadata doesn't leak
-- space after it's consumed to produce a message or exception
module Main where

import Data.Word (Word64)
import Sentry.Workload.Capture qualified as Workload
import Weigh (Weight)
import Weigh qualified

main :: IO ()
main = Weigh.mainWith do
  Weigh.setColumns [Weigh.Case, Weigh.Allocated, Weigh.Live, Weigh.Check]
  Weigh.validateAction
    "captureMessage, no scope data"
    (Workload.baselineMessage Workload.discardingClient)
    ()
    (maxLiveBytes residencyCeiling)
  Weigh.validateAction
    "captureException, no scope data"
    (Workload.baselineException Workload.discardingClient)
    ()
    (maxLiveBytes residencyCeiling)
  Weigh.validateAction
    "captureMessage, typical request scope"
    (Workload.runProfile Workload.discardingClient Workload.typical)
    ()
    (maxLiveBytes residencyCeiling)
  Weigh.validateAction
    "captureMessage, heavy request scope"
    (Workload.runProfile Workload.discardingClient Workload.heavy)
    ()
    (maxLiveBytes residencyCeiling)

-- | 50 kB maximum ceiling for heap residency; anything below this is pure
-- bookkeeping overhead that can vary by GHC version.
--
-- It's fine to adjust if a new library or compiler version adds a bit more
-- allocation padding since this accumulates over the lifetime of the benchmark
-- run but this should be stable outside of libraries changing their
-- representation or a newer version of GHC being a little heavier on allocation.
residencyCeiling :: Word64
residencyCeiling = 50_000

-- | Fail if live bytes exceed the given ceiling.
maxLiveBytes :: Word64 -> Weight -> Maybe String
maxLiveBytes n w
  | live > n = Just ("Live bytes exceeds " <> show n <> ": " <> show live)
  | otherwise = Nothing
  where
    live = Weigh.weightLiveBytes w
