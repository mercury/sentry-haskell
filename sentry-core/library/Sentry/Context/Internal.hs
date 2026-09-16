-- | Shared transformations of custom context payloads. Typed contexts are unchanged.
--
-- /This module's API is unstable!/ "Sentry.Context" should be preferred.
module Sentry.Context.Internal (removeValue, modifyValues, modifyTyped, lookupValue, alterValue, alterTyped) where

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
modifyTyped create k initial project inject f = Map.alter changeTyped k
  where
    apply value = let !result = f value in Just (inject result)
    changeTyped Nothing
      | create = apply initial
      | otherwise = Nothing
    changeTyped original@(Just context) = case project context of
      Nothing -> original
      Just value -> apply value

-- | Look up a custom field, ignoring typed payloads.
lookupValue :: Text -> Text -> Map Text Sentry.Context.Context -> Maybe Aeson.Value
lookupValue key field contexts = case Map.lookup key contexts of
  Just (Sentry.Context.Other values) -> Map.lookup field values
  _ -> Nothing

-- | Alter a custom field without evaluating callbacks for typed payloads.
-- Missing results do not create contexts; existing empty contexts are retained.
alterValue :: Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> Map Text Sentry.Context.Context -> Map Text Sentry.Context.Context
alterValue key field f = Map.alter alterOther key
  where
    alterOther Nothing = case f Nothing of
      Nothing -> Nothing
      Just value -> let !values = Map.singleton field value in Just (Sentry.Context.Other values)
    alterOther (Just (Sentry.Context.Other values)) =
      let !result = Map.alter f field values in Just (Sentry.Context.Other result)
    alterOther typed = typed

-- | Alter matching or absent payloads, preserving mismatches without calling the callback.
alterTyped :: Text -> (Sentry.Context.Context -> Maybe a) -> (a -> Sentry.Context.Context) -> (Maybe a -> Maybe a) -> Map Text Sentry.Context.Context -> Map Text Sentry.Context.Context
alterTyped key project inject f = Map.alter change key
  where
    apply value = case f value of
      Nothing -> Nothing
      Just !result -> Just (inject result)
    change Nothing = apply Nothing
    change original@(Just context) = case project context of
      Nothing -> original
      Just value -> apply (Just value)
