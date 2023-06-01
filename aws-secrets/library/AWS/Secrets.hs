module AWS.Secrets
  ( getSecret,
    getSecretOrFail,
    SecretsConfig,
    SecretReader,
    SecretKey,
    SecretName,
  )
where

import AWS.Secrets.Config (SecretsConfig)
import AWS.Secrets.Fetch (fetchSecret)
import AWS.Secrets.Key (SecretKey)
import AWS.Secrets.Name (SecretName)
import AWS.Secrets.Reader (SecretReader)
import qualified AWS.Secrets.Reader as Reader
import AWS.Secrets.SecretType (Secret, getSecretValue)
import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Fail (fail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (either)
import Data.Foldable (toList)
import Data.Function
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Validation (validation)
import System.IO (IO)

getSecret ::
  (MonadIO m, MonadError (Seq Text) m) =>
  SecretsConfig ->
  SecretName ->
  SecretReader a ->
  m a
getSecret config name reader = do
  s :: Secret <- modifyError Seq.singleton $ fetchSecret config name
  Reader.apply reader (getSecretValue s) & validation throwError pure

getSecretOrFail ::
  (MonadIO m) =>
  SecretsConfig ->
  SecretName ->
  SecretReader a ->
  m a
getSecretOrFail config name reader =
  runExceptT (getSecret config name reader)
    >>= either (liftIO . fail @IO . Text.unpack . Text.unlines . toList) pure

-- This can be gotten from "Control.Monad.Except" after upgrading to mtl 2.3.1
modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f m = runExceptT m >>= either (throwError . f) pure
