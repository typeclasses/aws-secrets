module AWS.Secrets.SecretType
  ( Secret,
    getSecretValue,
  )
where

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Prelude

-- | This type has a 'JSON.FromJSON' instance for use
-- with 'AWS.Secrets.Fetch.fetchSecret'.
data Secret = Secret
  { secretValue :: JSON.Object
  }

getSecretValue :: Secret -> JSON.Object
getSecretValue = secretValue

instance JSON.FromJSON Secret where
  parseJSON = \case
    JSON.Object v -> do
      SecretString ss <- v .: "SecretString"
      pure (Secret ss)
    invalid -> JSON.typeMismatch "Object" invalid

newtype SecretString = SecretString JSON.Object

instance JSON.FromJSON SecretString where
  parseJSON = \case
    JSON.String ss ->
      case decodeText @JSON.Object ss of
        Left e -> JSON.parserThrowError [] e
        Right x -> pure (SecretString x)
    invalid -> JSON.typeMismatch "String" invalid

decodeText :: forall a. JSON.FromJSON a => Text -> Either String a
decodeText = JSON.eitherDecode @a . Lazy.ByteString.fromStrict . encodeUtf8
