module MammutControl.API.GroupAPI
  ( GroupID(..)
  , WalletID(..)
  , Group(..)
  , getGroups
  ) where

import Prelude

import Data.Argonaut
import Data.Argonaut.Core
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Foreign.Object as Obj

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request as RQ
import Network.HTTP.Affjax.Response as RS
import Network.HTTP.StatusCode

import MammutControl.API.Helpers

newtype WalletID = WalletID Int

instance decodeJsonWalletID :: DecodeJson WalletID where
  decodeJson = map WalletID <<< decodeJson

newtype GroupID = GroupID Int

instance decodeJsonGroupID :: DecodeJson GroupID where
  decodeJson = map GroupID <<< decodeJson

data Group = Group
  { id          :: GroupID
  , name        :: String
  , description :: Maybe String
  , walletID    :: Maybe WalletID
  }

instance decodeJsonGroup :: DecodeJson Group where
  decodeJson json = do
    obj         <- decodeJson json
    id          <- obj .? "id"
    name        <- obj .? "name"
    description <- obj .?? "description"
    walletID    <- obj .?? "wallet_id"
    pure $ Group { id, name, description, walletID }

getGroups :: Aff (Either APIError (List Group))
getGroups = do
  response <- get' RS.json "/api/groups"
  processResponse response $ case response.status of
    StatusCode 200 -> pure $ do
      obj <- decodeJson response.response
      obj .? "groups"
    _ -> pure $ Left "Unknown error."
