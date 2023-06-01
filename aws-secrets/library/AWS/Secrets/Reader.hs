module AWS.Secrets.Reader
  ( -- * Type
    SecretReader (..),
    bind,

    -- * Examples
    text,
    string,
    scientific,
    boundedInteger,
  )
where

import AWS.Secrets.Key (SecretKey, getSecretKeyText)
import Control.Applicative (Applicative, pure)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import Data.Function (($), (.))
import Data.Functor (Functor, fmap)
import Data.Functor.Compose (Compose (..))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (Maybe (..), maybe)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Validation (Validation (..), bindValidation)
import Prelude (Bounded, Integral)

newtype SecretReader a = SecretReader
  { apply :: JSON.Object -> Validation (Seq Text) a
  }
  deriving
    (Functor, Applicative)
    via Compose ((->) JSON.Object) (Validation (Seq Text))

bind :: SecretReader a -> (a -> SecretReader b) -> SecretReader b
bind (SecretReader f) g = SecretReader \o ->
  f o `bindValidation` (($ o) . apply . g)

---

text :: SecretKey -> SecretReader Text
text k = SecretReader \o ->
  orFail ["Missing: " <> getSecretKeyText k] (KeyMap.lookup (key k) o)
    `bindValidation` \txt -> orFail ["Not a string: " <> getSecretKeyText k] (castJsonString txt)

string :: SecretKey -> SecretReader String
string = fmap Text.unpack . text

scientific :: SecretKey -> SecretReader Scientific
scientific k = SecretReader \o ->
  orFail ["Missing: " <> getSecretKeyText k] (KeyMap.lookup (key k) o)
    `bindValidation` \txt -> orFail ["Not a string: " <> getSecretKeyText k] (castJsonNumber txt)

boundedInteger :: (Integral i, Bounded i) => SecretKey -> SecretReader i
boundedInteger k =
  scientific k `bind` \s -> SecretReader \_ -> case Scientific.toBoundedInteger s of
    Nothing -> Failure ["Not an integer or not within bounds: " <> getSecretKeyText k]
    Just i -> pure i

---

key :: SecretKey -> JSON.Key
key = JSON.Key.fromText . getSecretKeyText

castJsonString :: JSON.Value -> Maybe Text
castJsonString = \case JSON.String x -> Just x; _ -> Nothing

castJsonNumber :: JSON.Value -> Maybe Scientific
castJsonNumber = \case JSON.Number x -> Just x; _ -> Nothing

orFail :: Seq Text -> Maybe a -> Validation (Seq Text) a
orFail err = maybe (Failure err) Success
