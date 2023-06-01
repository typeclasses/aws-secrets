module Main (main) where

import AWS.Secrets.SecretType (getSecretValue)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import System.IO (IO)
import Test.Hspec (describe, hspec, specify, shouldBe)

main :: IO ()
main = hspec do
  describe "Secret FromJSON" do
    specify "parses the example payload" do
      Just s <- liftIO (JSON.decodeFileStrict' "test/payload.json")
      let expected = "API key" .= ("fpqjd41Fja9nVnar3sd" :: Text)
      getSecretValue s `shouldBe` expected
