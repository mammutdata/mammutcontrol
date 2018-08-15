module MammutControl.Actions.UserActions
  ( LoginInfo(..)
  , UserCredentials(..)
  , signinAction
  , UserCreationData(..)
  , createUserAction
  , UserEditionData(..)
  , editUserAction
  , deleteUserAction
  ) where

import           Control.Monad.Base

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

import           Servant
import           Servant.Auth.Server

import           MammutControl.AccessControl
import           MammutControl.Actions.Helpers
import           MammutControl.Data.User
import           MammutControl.Data.Wallet

data LoginInfo = LoginInfo User BSL.ByteString

instance ToJSON LoginInfo where
  toJSON (LoginInfo uid token) = object
    [ "user"  .= uid
    , "token" .= TLE.decodeUtf8 token
    ]

data UserCredentials = UserCredentials T.Text BS.ByteString

instance FromJSON UserCredentials where
  parseJSON = withObject "user credentials" $ \obj -> UserCredentials
    <$> obj .: "email"
    <*> fmap TE.encodeUtf8 (obj .: "password")

signinAction :: (MonadBase IO m, MonadAction m)
             => JWTSettings -> UserCredentials -> m LoginInfo
signinAction jwtSettings (UserCredentials email password) =
  skipAccessControl $ do
    user <- getUserByEmail email
    validatePassword user password
    token <- makeSessionToken jwtSettings user
    return $ LoginInfo user token

data UserCreationData = UserCreationData T.Text T.Text BS.ByteString

instance FromJSON UserCreationData where
  parseJSON = withObject "user creation data" $ \obj -> UserCreationData
    <$> obj .: "email"
    <*> obj .: "name"
    <*> fmap TE.encodeUtf8 (obj .: "password")

createUserAction :: (MonadBase IO m, MonadAction m)
                 => JWTSettings -> UserCreationData -> m LoginInfo
createUserAction jwtSettings (UserCreationData email name password) = do
  let wallet = Wallet
        { walletID           = ()
        , walletName         = "personal"
        , walletDescription  = Just "This wallet was automatically\
                                    \ created together with your\
                                    \ account."
        , walletCredits      = ()
        , walletCreationTime = ()
        }
  user <- createUserFromData email name password
  _    <- asUser (userID user) $ createWallet wallet (userID user)
  token <- makeSessionToken jwtSettings user
  return $ LoginInfo user token

data UserEditionData = UserEditionData (Maybe BS.ByteString) (User' Maybe)

instance FromJSON UserEditionData where
  parseJSON = withObject "user edition data" $ \obj -> UserEditionData
    <$> fmap (fmap TE.encodeUtf8) (obj .:? "password")
    <*> parseJSON (Object obj)

editUserAction :: MonadAction m => UserID -> UserEditionData -> m User
editUserAction uid (UserEditionData mPassword fields) = do
  fields' <- case mPassword of
    Nothing -> return fields
    Just pwd -> do
      hash <- hashPassword pwd
      return $ fields { userPasswordHash = Just hash }
  editUser uid fields'

deleteUserAction :: MonadAction m => UserID -> m NoContent
deleteUserAction uid = do
  deleteUser uid
  return NoContent
