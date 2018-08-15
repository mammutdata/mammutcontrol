module MammutControl.API.UserAPI
  ( signin
  , signup
  ) where

import Prelude

import Data.Argonaut.Core as A
import Data.Either (Either(..))
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
import MammutControl.Session

signin :: { email :: String, password :: String }
       -> Aff (Either APIError Unit)
signin credentials = do
  let req = RQ.json $ A.fromObject $ Obj.fromFoldable
        [ Tuple "email"    $ A.fromString credentials.email
        , Tuple "password" $ A.fromString credentials.password
        ]
  response <- post RS.json "/api/users/signin" req
  processResponse response $ case response.status of
    StatusCode 200 -> liftEffect $ readSessionInfo response.response
    _ -> pure $ Left "unknown error"

signup :: { name :: String, email :: String, password :: String }
       -> Aff (Either APIError Unit)
signup credentials = do
  let req = RQ.json $ A.fromObject $ Obj.fromFoldable
        [ Tuple "name"     $ A.fromString credentials.name
        , Tuple "email"    $ A.fromString credentials.email
        , Tuple "password" $ A.fromString credentials.password
        ]
  response <- post RS.json "/api/users" req
  processResponse response $ case response.status of
    StatusCode 201 -> liftEffect $ readSessionInfo response.response
    _ -> pure $ Left "unknown error"

readSessionInfo :: A.Json -> Effect (Either String Unit)
readSessionInfo json = case parseSessionInfo json of
    Left  err   -> pure $ Left err
    Right token -> do
      setToken token
      pure $ Right unit

  where
    parseSessionInfo :: A.Json -> Either String String
    parseSessionInfo =
      A.caseJsonObject (Left "expected object for session info") \obj ->
        case Obj.lookup "token" obj of
          Nothing -> Left "no key token"
          Just tk ->
            A.caseJsonString (Left "expected string for token") Right tk
