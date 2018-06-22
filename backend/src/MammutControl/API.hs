module MammutControl.API
  ( MammutControlAPI
  , UnauthenticatedAPI  -- FIXME
  , api
  ) where

import Data.List.NonEmpty

import Servant hiding (throwError)
import Servant.Auth
import Servant.Auth.Server

import MammutControl.Actions.Helpers
import MammutControl.Actions.UserActions
import MammutControl.Actions.WalletActions
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
          :> Put '[JSON] User
  :<|> "users" :> Capture "user_id" UserID :> DeleteNoContent '[JSON] NoContent

  :<|> "users" :> Capture "user_id" UserID :> "wallets" :> Get '[JSON] [Wallet]
  :<|> "wallets" :> ReqBody '[JSON] WalletData :> PostCreated '[JSON] Wallet

unauthenticatedAPI :: JWTSettings -> ServerT UnauthenticatedAPI Action
unauthenticatedAPI jwtSettings =
  signinAction jwtSettings
  :<|> createUserAction jwtSettings

authenticatedAPI :: Session -> ServerT AuthenticatedAPI Action
authenticatedAPI session =
  editUserAction
  :<|> deleteUserAction

  :<|> getWalletsAction
  :<|> createWalletAction session

api :: Pool Connection -> IO Application
api pool = do
  key <- generateKey
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
               AccessDenied False "authentication required" :| []

      app = serveWithContext (Proxy :: Proxy (WithJSONErrors MammutControlAPI))
        context (unauthenticatedAPI' :<|> authenticatedAPI')

  return app
