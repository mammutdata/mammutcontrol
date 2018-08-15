module MammutControl.Data.UserGen where

import           Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

personNameGen :: Gen T.Text
personNameGen = do
  firstName <- Gen.text (Range.linear 1 10) Gen.alpha
  lastName  <- Gen.text (Range.linear 1 10) Gen.alpha
  return $ firstName <> " " <> lastName

emailGen :: Gen T.Text
emailGen = do
  name <- Gen.text (Range.linear 1 10) Gen.alphaNum
  return $ name <> "@email.tld"

passwordGen :: Gen BS.ByteString
passwordGen = Gen.utf8 (Range.linear 1 20) Gen.ascii
