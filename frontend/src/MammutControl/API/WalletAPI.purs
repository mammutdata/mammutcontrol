module MammutControl.API.WalletAPI
  ( WalletID(..)
  , unWalletID
  , Wallet(..)
  , getWallets
  ) where

import Prelude

import Data.Argonaut
import Data.Argonaut.Core
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Affjax
import Affjax.ResponseFormat as RF
import Affjax.StatusCode

import MammutControl.API.Helpers

newtype WalletID = WalletID String

unWalletID :: WalletID -> String
unWalletID (WalletID str) = str

instance encodeJsonWalletID :: EncodeJson WalletID where
  encodeJson = encodeJson <<< unWalletID

instance decodeJsonWalletID :: DecodeJson WalletID where
  decodeJson = map WalletID <<< decodeJson

data Wallet = Wallet
  { id          :: WalletID
  , name        :: String
  , description :: Maybe String
  , credits     :: Int
  }

instance decodeJsonWallet :: DecodeJson Wallet where
  decodeJson json = do
    obj         <- decodeJson json
    id          <- obj .? "id"
    name        <- obj .? "name"
    description <- obj .?? "description"
    credits     <- obj .? "credits"
    pure $ Wallet { id, name, description, credits }

getWallets :: Aff (Either APIError (Array Wallet))
getWallets = do
  response <- apiGet RF.json "/api/wallets"
  processResponse response \resp -> pure $ case resp.status of
    StatusCode 200 -> do
      obj <- decodeJson resp.body
      obj .? "wallets"
    _ -> Left "Unknown error."
