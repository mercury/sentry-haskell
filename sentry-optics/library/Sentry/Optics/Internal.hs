{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides orphan field labels for 'ScopeData', the 'edits'
-- combinator, the 'editScope' verb, and the @('&~')@ value-builder.
--
-- /Not a public module._ Import "Sentry.Optics" (qualified) or
-- "Sentry.Optics.Prelude" (unqualified) instead.
module Sentry.Optics.Internal
  ( editScope,
    edits,
    (&~),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (State, execState)
import Optics.TH (makeFieldLabelsFor)
import Patrol.Optics ()
import Sentry.Scope.Internal (Scope, ScopeData)
import Sentry.Scope.Update (ScopeUpdate (..), apply)

-- Orphan @OverloadedLabels@ field labels for the user-facing 'ScopeData' fields
-- like @#level@ and @#tags@.
--
-- The internal @type_@, @client@, and function-typed @eventProcessor@ fields
-- are deliberately omitted via the allowlist.
--
-- 'ScopeData' is defined in sentry-core, so these instances are orphans.
makeFieldLabelsFor
  [ ("level", "level"),
    ("fingerprint", "fingerprint"),
    ("transaction", "transaction"),
    ("breadcrumbs", "breadcrumbs"),
    ("user", "user"),
    ("extras", "extras"),
    ("tags", "tags"),
    ("contexts", "contexts")
  ]
  ''ScopeData

-- | Edit a 'Scope' in place with an imperative block of optic assignments over
-- its 'ScopeData', using @optics@' state operators (@.=@, @?=@, @%=@):
--
-- @
-- Sentry.withIsolationScope \\scope ->
--   Sentry.editScope scope do
--     #level ?= #warning
--     #tags % at "env" ?= "prod"
-- @
--
-- The block is a pure 'State' computation with no @IO@; it is collected into a
-- single 'ScopeUpdate' and applied as one atomic step.
--
-- This is the optics counterpart to the effectful @Sentry.Scope@ setters.
editScope :: (MonadIO m) => Scope -> State ScopeData () -> m ()
editScope scope = apply scope . edits

-- | Collect a block of optic assignments into a 'ScopeUpdate' without applying
-- it.
--
-- Shared with 'editScope', defined as @'apply' scope . 'edits'@.
edits :: State ScopeData () -> ScopeUpdate
edits = ScopeUpdate . execState

-- | Apply a block of optic assignments to a plain value, returning the updated
-- value as the value-level counterpart to 'editScope'.
--
-- Where 'editScope' applies such a block to a live 'Scope', @('&~')@ applies
-- one to any record, such as 'Patrol.Type.Breadcrumb.Breadcrumb' or
-- 'Patrol.Type.User.User', so you can build one up from an @empty@ value:
--
-- @
-- crumb = emptyBreadcrumb &~ do
--   #type_ ?= #ui
--   #category .= \"ui\"
--   #message .= \"user clicked 'pay'\"
-- @
--
-- The @optics@ library does not provide this operator, so it is defined here.
infixl 1 &~

(&~) :: s -> State s a -> s
(&~) = flip execState
