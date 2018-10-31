module MammutControl.API.GroupAPI
  ( GroupID(..)
  , unGroupID
  , Group(..)
  , GroupSummary(..)
  , getGroup
  , getGroups
  , createGroup
  , getMembersOfGroup
  , removeMemberFromGroup
  ) where

import Prelude

import Data.Argonaut
import Data.Argonaut.Core
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (null)
import Data.Tuple (Tuple(..))

import Foreign.Object as Obj

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Affjax
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode

import MammutControl.API.Helpers
import MammutControl.API.UserAPI (User, UserID, unUserID)
import MammutControl.API.WalletAPI (WalletID)

newtype GroupID = GroupID String

unGroupID :: GroupID -> String
unGroupID (GroupID str) = str

instance decodeJsonGroupID :: DecodeJson GroupID where
  decodeJson = map GroupID <<< decodeJson

data Group = Group
  { id           :: GroupID
  , name         :: String
  , description  :: Maybe String
  , walletID     :: Maybe WalletID
  }

data GroupSummary = GroupSummary Group Int Int

instance decodeJsonGroup :: DecodeJson Group where
  decodeJson json = do
    obj          <- decodeJson json
    id           <- obj .? "id"
    name         <- obj .? "name"
    description  <- obj .?? "description"
    walletID     <- obj .?? "wallet_id"
    pure $ Group { id, name, description, walletID }

instance decodeJsonGroupSummary :: DecodeJson GroupSummary where
  decodeJson json = do
    group        <- decodeJson json
    obj          <- decodeJson json
    userCount    <- obj .? "user_count"
    replicaCount <- obj .? "replica_count"
    pure $ GroupSummary group userCount replicaCount

getGroup :: GroupID -> Aff (Either APIError Group)
getGroup gid = do
  response <- apiGet RF.json $ "/api/groups/" <> unGroupID gid
  processResponse response \resp -> pure $ case resp.status of
    StatusCode 200 -> do
      obj <- decodeJson resp.body
      obj .? "group"
    _ -> Left "Unknown error."

getGroups :: Aff (Either APIError (Array GroupSummary))
getGroups = do
  response <- apiGet RF.json "/api/groups"
  processResponse response \resp -> pure $ case resp.status of
    StatusCode 200 -> do
      obj <- decodeJson resp.body
      obj .? "groups"
    _ -> Left "Unknown error."

createGroup :: { name :: String, description :: String
               , walletID :: Maybe WalletID | _ }
            -> Aff (Either APIError Group)
createGroup group = do
  let req = fromObject $ Obj.fromFoldable $
        [ "name" := group.name
        ] <> maybe [] (\wid -> ["wallet_id" := wid]) group.walletID
          <> if null group.description
               then []
               else ["description" := group.description]
  response <- apiPost RF.json "/api/groups" $ RB.json req
  processResponse response \resp -> pure $ case resp.status of
    StatusCode 201 -> do
      obj <- decodeJson resp.body
      obj .? "group"
    _ -> Left "Unknown error."

getMembersOfGroup :: GroupID -> Aff (Either APIError (Array User))
getMembersOfGroup gid = do
  response <- apiGet RF.json $ "/api/groups/" <> unGroupID gid <> "/users"
  processResponse response \resp -> pure $ case resp.status of
    StatusCode 200 -> do
      obj <- decodeJson resp.body
      obj .? "users"
    _ -> Left "Unknown error."

removeMemberFromGroup :: GroupID -> UserID -> Aff (Either APIError (Array User))
removeMemberFromGroup gid uid = do
  response <- apiDelete RF.json $
    "/api/groups/" <> unGroupID gid <> "/users/" <> unUserID uid
  processResponse response \resp -> pure $ case resp.status of
    StatusCode 200 -> do
      obj <- decodeJson resp.body
      obj .? "users"
    _ -> Left "Unknown error."
