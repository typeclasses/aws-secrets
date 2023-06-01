module AWS.Secrets.Name
  ( SecretName (..),
    getSecretNameText,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Prelude (Eq, Ord, Show)

newtype SecretName = SecretName Text
  deriving newtype (IsString, Show, Eq, Ord)

getSecretNameText :: SecretName -> Text
getSecretNameText (SecretName x) = x
