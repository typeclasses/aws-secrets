module AWS.Secrets.Key
  ( SecretKey (..),
    getSecretKeyText,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Prelude (Eq, Ord, Show)

-- | A secret can have multiple key/value pairs.
newtype SecretKey = SecretKeyText Text
  deriving newtype (IsString, Show, Eq, Ord)

getSecretKeyText :: SecretKey -> Text
getSecretKeyText (SecretKeyText x) = x
