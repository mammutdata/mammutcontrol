module MammutControl.Actions.Helpers
  ( module MammutControl.Actions.Helpers
  , module Servant.Server
  , throwError
  ) where

import           Control.Monad (forM_)
import           Control.Monad.Base
import           Control.Monad.MultiExcept
import qualified Control.Monad.Except as E

import           Data.Aeson
import           Data.Time (addUTCTime)
import qualified Data.ByteString.Lazy as BSL

import           System.IO

import           Servant.Auth.Server
import           Servant.Server

import           MammutControl.AccessControl
import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Data.Wallet
import           MammutControl.Error

type Action = AccessControlT DataM

type MonadAction m = ( MonadMultiError MCError m
                     , MonadTime m
                     , MonadTransaction m
                     , MonadUser m
                     , MonadWallet m
                     )

newtype Session = Session { sessionUserID :: UserID }

instance FromJWT Session
instance ToJWT   Session

instance FromJSON Session where
  parseJSON = withObject "session" $ \obj ->
    Session <$> obj .: "user_id"

instance ToJSON Session where
  toJSON Session{..} = object
    [ "user_id" .= sessionUserID
    ]

makeSessionToken :: (MonadBase IO m, MonadAction m)
                 => JWTSettings -> User -> m BSL.ByteString
makeSessionToken jwtSettings user = do
  time <- getTime
  let session = Session { sessionUserID = userID user }
  eRes <- liftBase $
    makeJWT session jwtSettings (Just (addUTCTime (7*24*3600) time))
  case eRes of
    Left  err   -> throwError $ AuthenticationError $ show err
    Right token -> return token

runAction :: Pool Connection -> Maybe Session -> Action a -> Handler a
runAction pool mSession action = do
  eRes <- liftBase $ flip runDataM pool $ withTransaction $
    runAccessControlT action (sessionUserID <$> mSession)
  case eRes of
    Left errs -> do
      forM_ errs $ \err ->
        liftBase $ hPutStrLn stderr $ "Error running action: " ++ show err
      E.throwError $ toServantErr errs
    Right res -> return res
