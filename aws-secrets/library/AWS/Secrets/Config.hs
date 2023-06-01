{- |

This library uses the AWS Command Line Interface. By default, this is
expected to be found as an executable called "aws" on the binary PATH.
This can be customized using 'setAwsCli'.

-}
module AWS.Secrets.Config
  ( SecretsConfig,
    secretsConfig,

    -- ** Region
    AwsRegion (..),
    getAwsRegion,
    getAwsRegionText,

    -- ** AWS command-line tool
    getAwsCli,
    setAwsCli,
    AwsCli (..),
    getAwsCliFilePath,
  )
where

import Data.Text (Text)
import System.IO (FilePath)

data SecretsConfig = SecretConfig
  { secretsConfigAwsCli :: AwsCli,
    secretsConfigAwsRegion :: AwsRegion
  }

newtype AwsCli = AwsCli FilePath

newtype AwsRegion = AwsRegion Text

defaultAwsCli :: AwsCli
defaultAwsCli = AwsCli "aws"

secretsConfig :: AwsRegion -> SecretsConfig
secretsConfig secretsConfigAwsRegion =
  SecretConfig {secretsConfigAwsCli = defaultAwsCli, secretsConfigAwsRegion}

setAwsCli :: AwsCli -> SecretsConfig -> SecretsConfig
setAwsCli secretsConfigAwsCli config = config {secretsConfigAwsCli}

getAwsCli :: SecretsConfig -> AwsCli
getAwsCli = secretsConfigAwsCli

getAwsRegion :: SecretsConfig -> AwsRegion
getAwsRegion = secretsConfigAwsRegion

getAwsCliFilePath :: AwsCli -> FilePath
getAwsCliFilePath (AwsCli x) = x

getAwsRegionText :: AwsRegion -> Text
getAwsRegionText (AwsRegion x) = x
