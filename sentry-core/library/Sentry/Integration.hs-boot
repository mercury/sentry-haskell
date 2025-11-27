module Sentry.Integration where

import Data.Kind (Constraint, Type)
import Type.Reflection (SomeTypeRep)

type Integration :: Type -> Constraint
class Integration t

type SomeIntegration :: Type
data SomeIntegration = forall t. Integration t => SomeIntegration SomeTypeRep t
