{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RoleAnnotations #-}

-- | The one composition idiom for pure metadata changes.
--
-- An @'Update' a@ is a pending change to an @a@: a wrapped endomorphism that
-- composes with '<>', applying the /left/ operand first, so later assignments
-- win on conflicting fields. 'mempty' is the update that changes nothing,
-- which allows conditional record updates:
--
-- @
-- import Sentry.User qualified
--
-- Sentry.User.setName name <> (if isInternal then betaCohort else mempty)
-- @
--
-- Optional record builders use @setX@ to replace, @modifyX@ to modify or
-- create from empty, and @modifyExistingX@ to modify only when present.
-- Existing-only builders skip absent values without evaluating the update.
-- Typed-context modifiers also skip mismatched variants; setters replace them.
--
-- Every record with builders gets its own qualified module, each of which
-- re-exports the record itself alongside @set@ \/ @unset@ \/ @modify@ \/ @add@
-- \/ @remove@ \/ @clear@ builders for its fields.
--
-- 'run', 'Sentry.updateScope', 'Sentry.Event.apply', 'Sentry.Breadcrumb.apply',
-- and nested-record builders all accept a single update, a list of updates,
-- or a replacement record.
--
-- Records and updates can also be converted in both directions:
--
-- @
-- alice :: User
-- alice = Witch.from [Sentry.User.setId \"42\", Sentry.User.setName \"Alice\"]
--
-- asUpdate :: UserUpdate
-- asUpdate = Witch.from alice
-- @
module Sentry.Update
  ( -- * Type
    Update (..),

    -- * Running an update
    run,

    -- * Reading in-progress values
    with,

    -- * Empty records
    Empty (..),
  )
where

import Data.Kind (Constraint, Type)
import Data.Monoid (Dual (..), Endo (..))
import Patrol qualified
import Patrol.Type.AppContext qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.BrowserContext qualified
import Patrol.Type.DeviceContext qualified
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Geo qualified as Patrol.Geo
import Patrol.Type.Mechanism qualified as Patrol.Mechanism
import Patrol.Type.OsContext qualified
import Patrol.Type.Request qualified
import Patrol.Type.RuntimeContext qualified as Patrol.RuntimeContext
import Patrol.Type.TraceContext qualified
import Patrol.Type.User qualified as Patrol.User
import Witch qualified

-- | A pending change to an @a@.
--
-- Compose with '<>'; the left operand is applied first, so later assignments
-- win.
--
-- Recover the wrapped @a -> a@ with 'runUpdate', or use 'run', which
-- additionally accepts a record or a list of updates.
type Update :: Type -> Type

type role Update representational
newtype Update a = Update {runUpdate :: a -> a}
  deriving (Semigroup, Monoid) via (Dual (Endo a))

-- | Records that have a canonical \"nothing assigned yet\" value, which is
-- what @'Witch.from' upd@ runs an update against.
--
-- Instances are hosted here, alongside the 'Witch.From' instances that need
-- them, so that neither is an orphan.
type Empty :: Type -> Constraint
class Empty a where
  -- | The record with no fields assigned.
  empty :: a

instance Empty Patrol.Breadcrumb where
  empty = Patrol.Breadcrumb.empty

instance Empty Patrol.Event where
  empty = Patrol.Event.empty

instance Empty Patrol.Geo where
  empty = Patrol.Geo.empty

instance Empty Patrol.Mechanism where
  empty = Patrol.Mechanism.empty

instance Empty Patrol.RuntimeContext where
  empty = Patrol.RuntimeContext.empty

instance Empty Patrol.User where
  empty = Patrol.User.empty

instance Empty Patrol.Request where
  empty = Patrol.Type.Request.empty

instance Empty Patrol.OsContext where
  empty = Patrol.Type.OsContext.empty

instance Empty Patrol.AppContext where
  empty = Patrol.Type.AppContext.empty

-- | @a -> 'Update' a@: assign a whole record, discarding anything assigned
-- before it.
instance Witch.From a (Update a) where
  from = Update . const

-- | @['Update' a] -> 'Update' a@: apply the updates in order.
--
-- Equivalent to 'mconcat', and what lets a list stand in for a '<>' chain
-- anywhere an update is expected.
instance Witch.From [Update a] (Update a) where
  from = mconcat

-- | @'Update' a -> a@: run the update against 'empty'.
--
-- To run it against some other base, use 'run'.
instance (Empty a) => Witch.From (Update a) a where
  from upd = runUpdate upd empty

-- | @['Update' a] -> a@: apply the updates in order, against 'empty'.
instance (Empty a) => Witch.From [Update a] a where
  from upds = runUpdate (mconcat upds) empty

-- | Run an update against a base record.
--
-- Accepts a single update, a list of them, or a replacement record.
run :: (Witch.From a (Update r)) => a -> r -> r
run upd = runUpdate (Witch.from upd)

-- | Build an update using the record's current value, including preceding
-- updates in the composition. Apply the resulting update to that same record.
--
-- Record-specific helpers such as 'Sentry.User.with' make record-dot type
-- inference easier.
with :: (Witch.From b (Update a)) => (a -> b) -> Update a
with k = Update \x -> run (k x) x

instance Empty Patrol.BrowserContext where
  empty = Patrol.Type.BrowserContext.empty

instance Empty Patrol.DeviceContext where
  empty = Patrol.Type.DeviceContext.empty

instance Empty Patrol.TraceContext where
  empty = Patrol.Type.TraceContext.empty
