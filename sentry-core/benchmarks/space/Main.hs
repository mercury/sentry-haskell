-- | Space benchmarks for the capture path.
module Main where

import Sentry.Workload.Capture qualified as Workload
import Weigh qualified

main :: IO ()
main = Weigh.mainWith do
  Weigh.setColumns [Weigh.Case, Weigh.Allocated, Weigh.GCs, Weigh.Live, Weigh.Max, Weigh.MaxOS]
  Weigh.io
    "captureMessage, no scope data"
    (Workload.baselineMessage Workload.discardingClient)
    ()
  Weigh.io
    "captureException, no scope data"
    (Workload.baselineException Workload.discardingClient)
    ()
  Weigh.io
    "captureMessage, typical request scope"
    (Workload.runProfile Workload.discardingClient Workload.typical)
    ()
  Weigh.io
    "captureMessage, heavy request scope"
    (Workload.runProfile Workload.discardingClient Workload.heavy)
    ()
