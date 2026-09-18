module Test.Fixtures (crumb, testUser) where

import Data.Text (Text)
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.User qualified as Patrol.User

-- | Build an otherwise empty breadcrumb with the given message.
crumb :: Text -> Patrol.Breadcrumb
crumb msg = Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = msg}

-- | A user with populated identity fields for metadata tests.
testUser :: Patrol.User
testUser =
  Patrol.User.User
    { data_ = mempty,
      email = "alice@example.com",
      geo = Nothing,
      id = "user-1",
      ipAddress = "",
      name = "Alice",
      segment = "",
      username = "alice"
    }
