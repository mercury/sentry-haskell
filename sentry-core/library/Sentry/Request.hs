-- | The Request record and composable field builders.
--
-- See "Sentry.Update" for details on our builder mechanism.
module Sentry.Request
  ( module Patrol.Type.Request,
    RequestUpdate,
    with,
    setUrl,
    setMethod,
    setFragment,
    setInferredContentType,
    setData,
    clearData,
    setCookie,
    setOptionalCookie,
    removeCookie,
    clearCookies,
    setHeader,
    setOptionalHeader,
    removeHeader,
    clearHeaders,
    setEnv,
    setOptionalEnv,
    removeEnv,
    clearEnv,
    setQueryParam,
    setOptionalQueryParam,
    removeQueryParam,
    clearQueryString,
    lookupCookie,
    lookupEnv,
    lookupQueryParam,
    lookupHeader,
  )
where

import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Patrol.Type.Request
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending record change.
type RequestUpdate :: Type
type RequestUpdate = Update Request

-- | Observe preceding updates and apply the resulting update to the same record.
with :: (Witch.From a RequestUpdate) => (Request -> a) -> RequestUpdate
with = Sentry.Update.with

-- | Assign url. Text fields can be cleared with empty text.
setUrl :: Text -> RequestUpdate
setUrl !assigned = Update \r -> r{Patrol.Type.Request.url = assigned}

-- | Assign method. Text fields can be cleared with empty text.
setMethod :: Text -> RequestUpdate
setMethod !assigned = Update \r -> r{Patrol.Type.Request.method = assigned}

-- | Assign fragment. Text fields can be cleared with empty text.
setFragment :: Text -> RequestUpdate
setFragment !assigned = Update \r -> r{Patrol.Type.Request.fragment = assigned}

-- | Assign inferredContentType. Text fields can be cleared with empty text.
setInferredContentType :: Text -> RequestUpdate
setInferredContentType !assigned = Update \r -> r{Patrol.Type.Request.inferredContentType = assigned}

-- | Assign data.
setData :: Aeson.Value -> RequestUpdate
setData !assigned = Update \r -> r{Patrol.Type.Request.data_ = assigned}

-- | Clear the body to JSON Null.
clearData :: RequestUpdate
clearData = setData Aeson.Null

-- | Insert a cookie; later assignments win.
setCookie :: Text -> Text -> RequestUpdate
setCookie !key !value = Update \r -> let !result = Map.insert key value r.cookies in r{Patrol.Type.Request.cookies = result}

-- | Remove a cookie.
removeCookie :: Text -> RequestUpdate
removeCookie !key = Update \r -> let !result = Map.delete key r.cookies in r{Patrol.Type.Request.cookies = result}

-- | Clear cookies.
clearCookies :: RequestUpdate
clearCookies = Update \r -> r{Patrol.Type.Request.cookies = Map.empty}

-- | Insert a header, removing ASCII case-insensitive matches and keeping
-- the supplied spelling. Whole-record replacement does not normalize headers.
setHeader :: Text -> Text -> RequestUpdate
setHeader !key !value = Update \r -> let !result = Map.insert key value (withoutHeader key r.headers) in r{Patrol.Type.Request.headers = result}

-- | Remove all ASCII case-insensitive matches for a header.
removeHeader :: Text -> RequestUpdate
removeHeader !key = Update \r -> let !result = withoutHeader key r.headers in r{Patrol.Type.Request.headers = result}

-- | Clear headers.
clearHeaders :: RequestUpdate
clearHeaders = Update \r -> r{Patrol.Type.Request.headers = Map.empty}

-- | Insert an environment entry; later assignments win.
setEnv :: Text -> Aeson.Value -> RequestUpdate
setEnv !key !value = Update \r -> let !result = Map.insert key value r.env in r{Patrol.Type.Request.env = result}

-- | Remove an environment entry.
removeEnv :: Text -> RequestUpdate
removeEnv !key = Update \r -> let !result = Map.delete key r.env in r{Patrol.Type.Request.env = result}

-- | Clear the environment map.
clearEnv :: RequestUpdate
clearEnv = Update \r -> r{Patrol.Type.Request.env = Map.empty}

-- | Insert a query parameter; later assignments win.
setQueryParam :: Text -> Text -> RequestUpdate
setQueryParam !key !value = Update \r -> let !result = Map.insert key value r.queryString in r{Patrol.Type.Request.queryString = result}

-- | Remove a query parameter.
removeQueryParam :: Text -> RequestUpdate
removeQueryParam !key = Update \r -> let !result = Map.delete key r.queryString in r{Patrol.Type.Request.queryString = result}

-- | Clear the query string.
clearQueryString :: RequestUpdate
clearQueryString = Update \r -> r{Patrol.Type.Request.queryString = Map.empty}

-- | Match only ASCII letters case-insensitively; preserve the latest spelling.
withoutHeader :: Text -> Map.Map Text Text -> Map.Map Text Text
withoutHeader key = Map.filterWithKey (\existing _ -> not (headerMatches key existing))

headerMatches :: Text -> Text -> Bool
headerMatches a b = asciiLower a == asciiLower b
  where
    asciiLower = Text.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalCookie :: Text -> Maybe Text -> RequestUpdate
setOptionalCookie key = maybe (removeCookie key) (setCookie key)

-- | Replace or remove every ASCII case-insensitive match for this header.
-- Replacement retains the supplied key spelling.
setOptionalHeader :: Text -> Maybe Text -> RequestUpdate
setOptionalHeader key = maybe (removeHeader key) (setHeader key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalEnv :: Text -> Maybe Aeson.Value -> RequestUpdate
setOptionalEnv key = maybe (removeEnv key) (setEnv key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalQueryParam :: Text -> Maybe Text -> RequestUpdate
setOptionalQueryParam key = maybe (removeQueryParam key) (setQueryParam key)

-- | Look up a stored entry by key.
lookupCookie :: Text -> Request -> Maybe Text
lookupCookie key record = Map.lookup key record.cookies

-- | Look up a stored entry by key.
lookupEnv :: Text -> Request -> Maybe Aeson.Value
lookupEnv key record = Map.lookup key record.env

-- | Look up a stored entry by key.
lookupQueryParam :: Text -> Request -> Maybe Text
lookupQueryParam key record = Map.lookup key record.queryString

-- | Prefer exact spelling, then the first ASCII case-insensitive match in
-- ascending stored-key order. Whole-record replacements retain duplicate spellings.
lookupHeader :: Text -> Request -> Maybe Text
lookupHeader key record = case Map.lookup key record.headers of
  Just value -> Just value
  Nothing -> snd <$> Foldable.find (headerMatches key . fst) (Map.toAscList record.headers)
