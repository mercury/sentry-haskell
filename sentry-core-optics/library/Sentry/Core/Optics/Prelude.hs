-- | A batteries-included optics surface for authoring scope updates. Import it
-- __unqualified, in place of__ @import Optics@:
--
-- @
-- import Sentry.Core.Optics qualified as Sentry
-- import Sentry.Core.Optics.Prelude
-- @
--
-- It re-exports the full @optics@ vocabulary and the @optics@ state operators
-- (@?=@ \/ @.=@ \/ @%=@) that drive @Sentry.editScope@ blocks, plus @('&~')@ for
-- running the same do-notation over a plain value. The
-- 'Sentry.Scope.Internal.ScopeData' and @patrol@ field\/constructor labels, plus
-- the enum /value/ labels (@#error@, @#navigation@; see "Sentry.Core.Optics.Values"),
-- come into scope with it.
--
-- __Do not import this alongside @import Optics@__ — it is a replacement, and
-- importing both unqualified would make the shared optics names ambiguous.
module Sentry.Core.Optics.Prelude
  ( module Optics,
    (?=),
    (.=),
    (%=),
    (&~),
  )
where

import Optics
import Optics.State.Operators ((%=), (.=), (?=))
import Sentry.Core.Optics.Internal ((&~))

-- 'Sentry.Core.Optics.Internal' also brings the orphan 'ScopeData' field
-- labels into scope; 'Sentry.Core.Optics.Values' brings the enum value labels.
import Sentry.Core.Optics.Values ()
