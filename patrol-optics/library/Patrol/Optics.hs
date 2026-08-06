{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Orphan 'Optics.Label.LabelOptic' instances for the @patrol@ Sentry
-- protocol types, enabling @OverloadedLabels@ access to their fields and
-- constructors:
--
-- @
-- import Optics.Core
--
-- user & #email .~ \"a\@b.com\"     -- record field (a lens)
-- ctx  ^? #_Device                 -- sum constructor (a prism)
-- @
--
-- Field-name collisions across @patrol@ records (e.g. @name@, @type_@,
-- @version@, @data_@) make plain per-field lenses unusable.
--
-- The type-directed @#label@ API resolves each field by the structure's type.
--
-- The instances are orphans by necessity, as the types are defined in @patrol@.
--
-- This module exports no names of its own; importing it brings the instances
-- into scope.
module Patrol.Optics () where

import Optics.TH (makeFieldLabelsNoPrefix, makePrismLabels)
import Patrol.Type.AppContext qualified as AppContext
import Patrol.Type.AppleDebugImage qualified as AppleDebugImage
import Patrol.Type.Breadcrumb qualified as Breadcrumb
import Patrol.Type.Breadcrumbs qualified as Breadcrumbs
import Patrol.Type.BrowserContext qualified as BrowserContext
import Patrol.Type.CError qualified as CError
import Patrol.Type.ClientReport qualified as ClientReport
import Patrol.Type.ClientSdkInfo qualified as ClientSdkInfo
import Patrol.Type.ClientSdkPackage qualified as ClientSdkPackage
import Patrol.Type.Context qualified as Context
import Patrol.Type.DebugImage qualified as DebugImage
import Patrol.Type.DebugMeta qualified as DebugMeta
import Patrol.Type.DeviceContext qualified as DeviceContext
import Patrol.Type.DiscardedEvent qualified as DiscardedEvent
import Patrol.Type.Dsn qualified as Dsn
import Patrol.Type.Envelope qualified as Envelope
import Patrol.Type.Event qualified as Event
import Patrol.Type.EventProcessingError qualified as EventProcessingError
import Patrol.Type.Exception qualified as Exception
import Patrol.Type.Exceptions qualified as Exceptions
import Patrol.Type.Frame qualified as Frame
import Patrol.Type.Geo qualified as Geo
import Patrol.Type.GpuContext qualified as GpuContext
import Patrol.Type.Headers qualified as Headers
import Patrol.Type.Item qualified as Item
import Patrol.Type.Items qualified as Items
import Patrol.Type.LogEntry qualified as LogEntry
import Patrol.Type.MachException qualified as MachException
import Patrol.Type.Mechanism qualified as Mechanism
import Patrol.Type.MechanismMeta qualified as MechanismMeta
import Patrol.Type.NativeDebugImage qualified as NativeDebugImage
import Patrol.Type.NsError qualified as NsError
import Patrol.Type.OsContext qualified as OsContext
import Patrol.Type.PosixSignal qualified as PosixSignal
import Patrol.Type.ProguardDebugImage qualified as ProguardDebugImage
import Patrol.Type.Request qualified as Request
import Patrol.Type.Response qualified as Response
import Patrol.Type.RuntimeContext qualified as RuntimeContext
import Patrol.Type.Stacktrace qualified as Stacktrace
import Patrol.Type.SystemSdkInfo qualified as SystemSdkInfo
import Patrol.Type.Thread qualified as Thread
import Patrol.Type.Threads qualified as Threads
import Patrol.Type.TraceContext qualified as TraceContext
import Patrol.Type.TransactionInfo qualified as TransactionInfo
import Patrol.Type.User qualified as User

-- * Record field labels

makeFieldLabelsNoPrefix ''AppContext.AppContext
makeFieldLabelsNoPrefix ''AppleDebugImage.AppleDebugImage
makeFieldLabelsNoPrefix ''Breadcrumb.Breadcrumb
makeFieldLabelsNoPrefix ''Breadcrumbs.Breadcrumbs
makeFieldLabelsNoPrefix ''BrowserContext.BrowserContext
makeFieldLabelsNoPrefix ''CError.CError
makeFieldLabelsNoPrefix ''ClientReport.ClientReport
makeFieldLabelsNoPrefix ''ClientSdkInfo.ClientSdkInfo
makeFieldLabelsNoPrefix ''ClientSdkPackage.ClientSdkPackage
makeFieldLabelsNoPrefix ''DebugMeta.DebugMeta
makeFieldLabelsNoPrefix ''DeviceContext.DeviceContext
makeFieldLabelsNoPrefix ''DiscardedEvent.DiscardedEvent
makeFieldLabelsNoPrefix ''Dsn.Dsn
makeFieldLabelsNoPrefix ''Envelope.Envelope
makeFieldLabelsNoPrefix ''Event.Event
makeFieldLabelsNoPrefix ''EventProcessingError.EventProcessingError
makeFieldLabelsNoPrefix ''Exception.Exception
makeFieldLabelsNoPrefix ''Exceptions.Exceptions
makeFieldLabelsNoPrefix ''Frame.Frame
makeFieldLabelsNoPrefix ''Geo.Geo
makeFieldLabelsNoPrefix ''GpuContext.GpuContext
makeFieldLabelsNoPrefix ''Headers.Headers
makeFieldLabelsNoPrefix ''LogEntry.LogEntry
makeFieldLabelsNoPrefix ''MachException.MachException
makeFieldLabelsNoPrefix ''Mechanism.Mechanism
makeFieldLabelsNoPrefix ''MechanismMeta.MechanismMeta
makeFieldLabelsNoPrefix ''NativeDebugImage.NativeDebugImage
makeFieldLabelsNoPrefix ''NsError.NsError
makeFieldLabelsNoPrefix ''OsContext.OsContext
makeFieldLabelsNoPrefix ''PosixSignal.PosixSignal
makeFieldLabelsNoPrefix ''ProguardDebugImage.ProguardDebugImage
makeFieldLabelsNoPrefix ''Request.Request
makeFieldLabelsNoPrefix ''Response.Response
makeFieldLabelsNoPrefix ''RuntimeContext.RuntimeContext
makeFieldLabelsNoPrefix ''Stacktrace.Stacktrace
makeFieldLabelsNoPrefix ''SystemSdkInfo.SystemSdkInfo
makeFieldLabelsNoPrefix ''Thread.Thread
makeFieldLabelsNoPrefix ''Threads.Threads
makeFieldLabelsNoPrefix ''TraceContext.TraceContext
makeFieldLabelsNoPrefix ''TransactionInfo.TransactionInfo
makeFieldLabelsNoPrefix ''User.User

-- * Sum constructor labels (prisms)

makePrismLabels ''Context.Context
makePrismLabels ''DebugImage.DebugImage
makePrismLabels ''Item.Item
makePrismLabels ''Items.Items
