-- | The TraceContext record and composable field builders. See "Sentry.Update".
module Sentry.TraceContext
  ( module Patrol.Type.TraceContext,
    SpanStatus (..),
    TraceContextUpdate,
    with,
    setExclusiveTime,
    unsetExclusiveTime,
    setOp,
    setParentSpanId,
    setSpanId,
    setStatus,
    unsetStatus,
    setTraceId,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Patrol.Type.SpanStatus (SpanStatus (..))
import Patrol.Type.TraceContext
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending record change.
type TraceContextUpdate :: Type
type TraceContextUpdate = Update TraceContext

-- | Observe preceding updates and apply the resulting update to the same record.
with :: (Witch.From a TraceContextUpdate) => (TraceContext -> a) -> TraceContextUpdate
with = Sentry.Update.with

-- | Assign exclusiveTime.
setExclusiveTime :: Int -> TraceContextUpdate
setExclusiveTime !assigned = Update \r -> r{Patrol.Type.TraceContext.exclusiveTime = Just assigned}

-- | Clear exclusiveTime.
unsetExclusiveTime :: TraceContextUpdate
unsetExclusiveTime = Update \r -> r{Patrol.Type.TraceContext.exclusiveTime = Nothing}

-- | Assign op. Text fields can be cleared with empty text.
setOp :: Text -> TraceContextUpdate
setOp !assigned = Update \r -> r{Patrol.Type.TraceContext.op = assigned}

-- | Assign parentSpanId. Text fields can be cleared with empty text.
setParentSpanId :: Text -> TraceContextUpdate
setParentSpanId !assigned = Update \r -> r{Patrol.Type.TraceContext.parentSpanId = assigned}

-- | Assign spanId. Text fields can be cleared with empty text.
setSpanId :: Text -> TraceContextUpdate
setSpanId !assigned = Update \r -> r{Patrol.Type.TraceContext.spanId = assigned}

-- | Assign status.
setStatus :: SpanStatus -> TraceContextUpdate
setStatus !assigned = Update \r -> r{Patrol.Type.TraceContext.status = Just assigned}

-- | Clear status.
unsetStatus :: TraceContextUpdate
unsetStatus = Update \r -> r{Patrol.Type.TraceContext.status = Nothing}

-- | Assign traceId. Text fields can be cleared with empty text.
setTraceId :: Text -> TraceContextUpdate
setTraceId !assigned = Update \r -> r{Patrol.Type.TraceContext.traceId = assigned}
