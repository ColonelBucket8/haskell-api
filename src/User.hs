module User where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User
    { userId :: Text,
      userName :: Text 
    }

data CreateUserRequest = CreateUserRequest
    { name :: Text,
      password :: Text 
    }
    deriving (Generic)

instance FromJSON CreateUserRequest