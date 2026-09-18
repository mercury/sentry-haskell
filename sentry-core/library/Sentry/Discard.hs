-- | Best-effort notifications for items discarded locally by the SDK.
module Sentry.Discard (Callback, notify, itemCounts, recordItems, recordEnvelope) where

import Control.Exception (evaluate, mask)
import Control.Exception.Safe (catchAny)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Patrol.Type.Envelope qualified
import Patrol.Type.Items qualified as Items
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.ClientReport qualified as ClientReport

-- | Receive the reason, category, and positive number of discarded items.
type Callback :: Type
type Callback = DiscardReason -> DataCategory -> Int -> IO ()

-- | Notify without retrying, ignoring nonpositive quantities.
notify :: Maybe Callback -> DiscardReason -> DataCategory -> Int -> IO ()
notify callback reason category quantity
  | quantity <= 0 = pure ()
  | otherwise = for_ callback $ \call -> call reason category quantity `catchAny` \_ -> pure ()

-- | Group classified items by category, excluding reports and raw items.
itemCounts :: [Patrol.Item] -> [(DataCategory, Int)]
itemCounts items = Map.toList $ Map.fromListWith (+) [(category, 1) | item <- items, Just category <- [ClientReport.categoryFromItem item]]

-- | Record the whole batch before notifying, with no locks held during callbacks.
-- Only the bounded internal counter updates run masked. Notifications use the
-- caller's masking state and work independently of client-report configuration.
recordItems :: Maybe ClientReports -> Maybe Callback -> DiscardReason -> [Patrol.Item] -> IO ()
recordItems Nothing Nothing _ _ = pure ()
recordItems reports callback reason items = do
  let counts = itemCounts items
  -- Finish classification before masking; the remaining updates are bounded
  -- by the number of categories rather than the size of the envelope.
  _ <- evaluate (length counts)
  mask $ \restore -> do
    for_ reports $ \cr -> for_ counts $ \(category, quantity) -> ClientReport.record cr reason category quantity
    restore $ for_ counts $ \(category, quantity) -> notify callback reason category quantity

-- | Account for classified envelope items; unclassified raw payloads are ignored.
recordEnvelope :: Maybe ClientReports -> Maybe Callback -> DiscardReason -> Patrol.Envelope -> IO ()
recordEnvelope reports callback reason envelope = case envelope.items of
  Items.Raw _ -> pure ()
  Items.EnvelopeItems items -> recordItems reports callback reason items
