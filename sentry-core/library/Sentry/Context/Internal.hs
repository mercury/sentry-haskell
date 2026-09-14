-- | Shared transformations of custom context payloads. Typed contexts are unchanged.
--
-- /This module's API is unstable!/ "Sentry.Context" should be preferred.
module Sentry.Context.Internal (removeValue, modifyValues, modifyTyped) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Sentry.Context qualified

-- | Preserve absence and retain empty custom contexts.
removeValue :: Text -> Text -> Map Text Sentry.Context.Context -> Map Text Sentry.Context.Context
removeValue k key = Map.adjust remove k
  where
    remove (Sentry.Context.Other values) =
      let !result = Map.delete key values in Sentry.Context.Other result
    remove typed = typed

-- | Start from empty when absent; never evaluate the transformation for typed contexts.
modifyValues :: Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> Map Text Sentry.Context.Context -> Map Text Sentry.Context.Context
modifyValues k f = Map.alter alterOther k
  where
    alterOther = \case
      Nothing -> let !values = f Map.empty in Just (Sentry.Context.Other values)
      Just (Sentry.Context.Other m) -> let !values = f m in Just (Sentry.Context.Other values)
      typed -> typed

-- | Modify a matching payload, optionally creating an absent one. Mismatches
-- and skipped absences never evaluate the payload transformation.
modifyTyped :: Bool -> Text -> a -> (Sentry.Context.Context -> Maybe a) -> (a -> Sentry.Context.Context) -> (a -> a) -> Map Text Sentry.Context.Context -> Map Text Sentry.Context.Context
modifyTyped create k initial project inject f = Map.alter alterTyped k
  where
    apply value = let !result = f value in Just (inject result)
    alterTyped Nothing
      | create = apply initial
      | otherwise = Nothing
    alterTyped original@(Just context) = case project context of
      Nothing -> original
      Just value -> apply value
