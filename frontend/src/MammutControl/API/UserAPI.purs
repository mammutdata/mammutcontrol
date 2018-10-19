module MammutControl.API.UserAPI
  ( UserID(..)
  , unUserID
  , User(..)
  , signin
  , signup
  ) where

import Prelude

import Data.Argonaut
import Data.Argonaut.Core as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Foreign.Object as Obj

import Affjax
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode

import MammutControl.API.Helpers
import MammutControl.Session

newtype UserID = UserID String

unUserID :: UserID -> String
unUserID (UserID uid) = uid

instance decodeJsonUserID :: DecodeJson UserID where
  decodeJson = map UserID <<< decodeJson

data User = User
  { userID :: UserID
  , email  :: String
  }

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    userID <- obj .? "id"
    email  <- obj .? "email"
    pure $ User { userID, email }

signin :: { email :: String, password :: String | _ }
       -> Aff (Either APIError Unit)
signin credentials = do
  let payload = fromObject $ Obj.fromFoldable
        [ "email"    := credentials.email
        , "password" := credentials.password
        ]
  response <- apiPost RF.json "/api/users/signin" $ RB.json payload
  processResponse response \resp -> case resp.status of
    StatusCode 200 -> liftEffect $ readSessionInfo resp.body
    _ -> pure $ Left "unknown error"

signup :: { name :: String, email :: String, password :: String | _ }
       -> Aff (Either APIError Unit)
signup credentials = do
  let payload = A.fromObject $ Obj.fromFoldable
        [ "name"     := credentials.name
        , "email"    := credentials.email
        , "password" := credentials.password
        ]
  response <- apiPost RF.json "/api/users" $ RB.json payload
  processResponse response \resp -> case resp.status of
    StatusCode 201 -> liftEffect $ readSessionInfo resp.body
    _ -> pure $ Left "unknown error"

readSessionInfo :: A.Json -> Effect (Either String Unit)
readSessionInfo json = case parseSessionInfo json of
    Left  err   -> pure $ Left err
    Right token -> do
      setToken token
      pure $ Right unit

  where
    parseSessionInfo :: A.Json -> Either String String
    parseSessionInfo val = do
      obj <- decodeJson val
      obj .? "token"
