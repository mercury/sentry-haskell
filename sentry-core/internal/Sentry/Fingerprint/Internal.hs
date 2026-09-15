-- | Private fingerprint list operations. Lists and components remain shallow.
module Sentry.Fingerprint.Internal where

import Data.Text (Text)

-- | Canonical Sentry default-grouping placeholder.
defaultFingerprintComponent :: Text
defaultFingerprintComponent = "{{ default }}"

isDefault :: Text -> Bool
isDefault value = value == defaultFingerprintComponent || value == "{{default}}"

ensureDefault :: [Text] -> [Text]
ensureDefault values
  | any isDefault values = values
  | otherwise = defaultFingerprintComponent : values

removeDefault :: [Text] -> [Text]
removeDefault = filter (not . isDefault)

prepend :: Text -> [Text] -> [Text]
prepend !value values = value : values

append :: Text -> [Text] -> [Text]
append !value values = values <> [value]

-- | Only empty and singleton default fingerprints defer to scope grouping.
merge :: Maybe [Text] -> [Text] -> [Text]
merge scope event
  | fallback event = maybe event id scope
  | otherwise = event
  where
    fallback [] = True
    fallback [value] = isDefault value
    fallback _ = False
