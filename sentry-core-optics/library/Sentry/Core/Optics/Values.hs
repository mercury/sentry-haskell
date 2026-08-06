{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Orphan 'IsLabel' instances that expose @patrol@ enum /values/ as
-- @OverloadedLabels@:
--
-- @
-- #level ?= #error              -- #error :: Level (pinned by the optic)
-- #type_ ?~ #navigation         -- #navigation :: BreadcrumbType
-- @
--
-- These are deliberately for the optics path only: a value label like @#error@
-- carries no type of its own, so it is only usable where the surrounding optic
-- determines the type (a setter target, a function argument). That is exactly
-- how they are used in @editScope@ blocks and setters, so they are unambiguous in
-- practice — and in the rare standalone position, reach for the qualified
-- "Sentry.Level" \/ "Sentry.BreadcrumbType" constructors instead.
--
-- They are plain /specific/ instances, so they do not overlap @optics@' generic
-- @IsLabel name (Optic …)@ instance (a concrete enum type can't unify with an
-- @Optic@), and a shared label like @#error@ resolves to whichever enum the
-- optic expects. Orphans (the @IsLabel@ class is from @base@, the enums from
-- @patrol@); importing this module brings them into scope.
module Sentry.Core.Optics.Values () where

import GHC.OverloadedLabels (IsLabel (..))
import Patrol.Type.BreadcrumbType qualified as BreadcrumbType
import Patrol.Type.Level qualified as Level

-- * Level

instance IsLabel "debug" Level.Level where fromLabel = Level.Debug

instance IsLabel "info" Level.Level where fromLabel = Level.Info

instance IsLabel "warning" Level.Level where fromLabel = Level.Warning

instance IsLabel "error" Level.Level where fromLabel = Level.Error

instance IsLabel "fatal" Level.Level where fromLabel = Level.Fatal

-- * BreadcrumbType

instance IsLabel "default" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Default

instance IsLabel "debug" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Debug

instance IsLabel "error" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Error

instance IsLabel "navigation" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Navigation

instance IsLabel "http" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Http

instance IsLabel "info" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Info

instance IsLabel "query" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Query

instance IsLabel "transaction" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.Transaction

instance IsLabel "ui" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.UI

instance IsLabel "user" BreadcrumbType.BreadcrumbType where fromLabel = BreadcrumbType.User
