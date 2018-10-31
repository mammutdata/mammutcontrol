module MammutControl.API
  ( MammutControlAPI
  , UnauthenticatedAPI  -- FIXME
  , api
  ) where

import Data.List.NonEmpty

import Crypto.JOSE.JWK (JWK)

import Servant hiding (throwError)
import Servant.Auth
import Servant.Auth.Server

import MammutControl.Actions.GroupActions
import MammutControl.Actions.Helpers
import MammutControl.Actions.UserActions
import MammutControl.Actions.WalletActions
import MammutControl.Data.Group
import MammutControl.Data.Types
import MammutControl.Data.User
import MammutControl.Data.Wallet
import MammutControl.Error

type MammutControlAPI =
  UnauthenticatedAPI :<|> Auth '[JWT] Session :> AuthenticatedAPI

type UnauthenticatedAPI =
  "users" :> "signin" :> ReqBody '[JSON] UserCredentials
          :> Post '[JSON] LoginInfo

  :<|> "users" :> ReqBody '[JSON] UserCreationData
               :> PostCreated '[JSON] LoginInfo

type AuthenticatedAPI =
  "users" :> Capture "user_id" UserID :> ReqBody '[JSON] UserEditionData
          :> Put '[JSON] (JSONWrapper "user" User)
  :<|> "users" :> Capture "user_id" UserID :> DeleteNoContent '[JSON] NoContent

  :<|> "wallets" :> Get '[JSON] (JSONWrapper "wallets" [Wallet])
  :<|> "wallets" :> Capture "wallet_id" WalletID
                 :> Get '[JSON] (JSONWrapper "wallet" Wallet)
  :<|> "wallets" :> ReqBody '[JSON] WalletData
                 :> PostCreated '[JSON] (JSONWrapper "wallet" Wallet)

  :<|> "groups" :> Get '[JSON] (JSONWrapper "groups" [GroupSummary])
  :<|> "groups" :> Capture "group_id" GroupID
                :> Get '[JSON] (JSONWrapper "group" Group)
  :<|> "groups" :> ReqBody '[JSON] GroupData
                :> PostCreated '[JSON] (JSONWrapper "group" Group)
  :<|> "groups" :> Capture "group_id" GroupID :> "users"
                :> Get '[JSON] (JSONWrapper "users" [User])
  :<|> "groups" :> Capture "group_id" GroupID :> "users"
                :> ReqBody '[JSON] GroupMembershipData
                :> Post '[JSON] (JSONWrapper "users" [User])
  :<|> "groups" :> Capture "group_id" GroupID :> "users"
                :> Capture "user_id" UserID
                :> Delete '[JSON] (JSONWrapper "users" [User])

unauthenticatedAPI :: JWTSettings -> ServerT UnauthenticatedAPI Action
unauthenticatedAPI jwtSettings =
  signinAction jwtSettings
  :<|> createUserAction jwtSettings

authenticatedAPI :: Session -> ServerT AuthenticatedAPI Action
authenticatedAPI session =
  editUserAction
  :<|> deleteUserAction

  :<|> getWalletsAction session
  :<|> getWalletAction
  :<|> createWalletAction session

  :<|> getGroupsAction session
  :<|> getGroupAction
  :<|> createGroupAction session
  :<|> getMembersOfGroupAction
  :<|> addUserToGroupAction
  :<|> removeUserFromGroupAction

api :: JWK -> Pool Connection -> Application
api key pool =
  let contextProxy = Proxy :: Proxy '[JWTSettings, CookieSettings]

      jwtSettings = defaultJWTSettings key
      authCheck = jwtAuthCheck jwtSettings :: AuthCheck Session
      context =
        jwtSettings :. defaultCookieSettings :. authCheck :. EmptyContext

      unauthenticatedAPI' = hoistServerWithContext
        (Proxy :: Proxy UnauthenticatedAPI) contextProxy
        (runAction pool Nothing) (unauthenticatedAPI jwtSettings)

      authenticatedAPI' = \case
        Authenticated session -> hoistServerWithContext
          (Proxy :: Proxy AuthenticatedAPI) contextProxy
          (runAction pool (Just session)) (authenticatedAPI session)
        _ -> throwAll $ toServantErr $
               AccessDenied Invalid "authentication required" :| []

      app = serveWithContext (Proxy :: Proxy (WithJSONErrors MammutControlAPI))
        context (unauthenticatedAPI' :<|> authenticatedAPI')

  in app
