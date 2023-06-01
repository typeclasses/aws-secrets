module AWS.Secrets.Fetch where

import AWS.Secrets.Config (SecretsConfig)
import qualified AWS.Secrets.Config as Config
import AWS.Secrets.Name (SecretName, getSecretNameText)
import Control.Applicative (pure)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Either (Either (..))
import Data.Foldable (fold)
import Data.Function (($), (.))
import Data.Int (Int)
import qualified Data.List as List
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified System.Exit as Exit
import System.IO (FilePath)
import qualified System.Process.Typed as Process
import Text.Show (Show, show)

-- | Type @result@ may be e.g. 'AWS.Secrets.SecretType.Secret'.
fetchSecret ::
  forall m result.
  (MonadIO m, MonadError Text m, JSON.FromJSON result) =>
  SecretsConfig ->
  SecretName ->
  m result
fetchSecret config name = do
  let secretNameText :: Text
      secretNameText = getSecretNameText name

      secretNameString :: String
      secretNameString = Text.unpack secretNameText

      secretNameTextBuilder :: Text.Builder
      secretNameTextBuilder = Text.Builder.fromText secretNameText

      awsRegionText :: Text
      awsRegionText = Config.getAwsRegionText (Config.getAwsRegion config)

      awsRegionString :: String
      awsRegionString = Text.unpack awsRegionText

      awsRegionTextBuilder :: Text.Builder
      awsRegionTextBuilder = Text.Builder.fromText awsRegionText

      executableFilePath :: FilePath
      executableFilePath = Config.getAwsCliFilePath (Config.getAwsCli config)

      executableTextBuilder :: Text.Builder
      executableTextBuilder = Text.Builder.fromString executableFilePath

      stringArgs :: [String]
      stringArgs =
        [ "secretsmanager",
          "get-secret-value",
          "--secret-id",
          secretNameString,
          "--region",
          awsRegionString
        ]

      fullCommandTextBuilder :: Text.Builder
      fullCommandTextBuilder =
        unwords
          [ "The exact command executed was:",
            showBuilder @[String] (executableFilePath : stringArgs)
          ]

      descriptionTextBuilder :: Text.Builder
      descriptionTextBuilder =
        unwords
          [ "AWS command",
            quote executableTextBuilder,
            "to get secret",
            quote secretNameTextBuilder,
            "from region",
            quote $ awsRegionTextBuilder
          ]

  (exitCode :: Exit.ExitCode, output :: Lazy.ByteString, error :: Lazy.ByteString) <-
    Process.readProcess (Process.proc executableFilePath stringArgs)

  let -- Shows what came out on stdout
      normalOutputMessage :: Text.Builder
      normalOutputMessage =
        if Lazy.ByteString.null output
          then "It produced no output."
          else
            unwords
              [ "Its output was:",
                showBuilder @Lazy.ByteString output
              ]

      -- Shows what came out on stderr
      errorOutputMessage :: Text.Builder
      errorOutputMessage =
        if Lazy.ByteString.null error
          then "It produced no error output."
          else
            unwords
              [ "Its error output was:",
                showBuilder @Lazy.ByteString error
              ]

      -- What to do if the JSON on stdout couldn't be parsed
      throwParseError :: forall x. String -> m x
      throwParseError parseError =
        (throwError . render . unlines)
          [ unwords
              [ descriptionTextBuilder,
                "failed to produce valid JSON"
              ],
            fullCommandTextBuilder,
            normalOutputMessage,
            unwords
              [ "The output from the parser is:",
                Text.Builder.fromString parseError
              ]
          ]

      -- What to do if AWS command returns a failure exit code
      throwExitCodeError :: forall x. Int -> m x
      throwExitCodeError exitCodeInt =
        (throwError . render . unlines)
          [ unwords
              [ descriptionTextBuilder,
                "failed with exit code",
                quote (showBuilder @Int exitCodeInt)
              ],
            fullCommandTextBuilder,
            errorOutputMessage
          ]

  case exitCode of
    Exit.ExitSuccess -> case JSON.eitherDecode @result output of
      Right x -> pure x
      Left e -> throwParseError e
    Exit.ExitFailure exitCodeInt ->
      throwExitCodeError exitCodeInt

quote :: Text.Builder -> Text.Builder
quote x = "‘" <> x <> "’"

unwords :: [Text.Builder] -> Text.Builder
unwords = fold . List.intersperse " "

unlines :: [Text.Builder] -> Text.Builder
unlines = fold . List.intersperse "\n"

showBuilder :: Show a => a -> Text.Builder
showBuilder = Text.Builder.fromString . show

render :: Text.Builder -> Text
render = Lazy.Text.toStrict . Text.Builder.toLazyText
