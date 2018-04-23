module MammutControl.Actions.Helpers
  ( module MammutControl.Actions.Helpers
  , module Servant.Server
  , throwError
  ) where

import           Control.Eff
import           Control.Eff.Exception (Exc, runError, throwError)
import           Control.Eff.Lift (Lift, lift, runLift)
import           Control.Monad.Trans (liftIO)
import qualified Control.Monad.Except as Except

import           Data.Aeson
import           Data.Time (addUTCTime)
import qualified Data.ByteString.Lazy as BSL

import           Servant.Auth.Server
import           Servant.Server

import           MammutControl.AccessControl
import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Error
import           MammutControl.Time

type ActionConstraints r = ( Member (Exc MCError) r, Member Time r
                           , Member UserAccess r )

type Action a = forall r. ActionConstraints r => Eff r a

type ConcreteAction = Eff '[Exc MCError, Time, UserAccess, Lift Handler]

newtype Session = Session
  { sessionUserID :: UserID
  }

instance FromJWT Session
instance ToJWT   Session

instance FromJSON Session where
  parseJSON = withObject "session" $ \obj ->
    Session <$> obj .: "user_id"

instance ToJSON Session where
  toJSON Session{..} = object
    [ "user_id" .= sessionUserID
    ]

makeSessionToken :: JWTSettings -> User -> ConcreteAction BSL.ByteString
makeSessionToken jwtSettings user = do
  time <- getTime
  let session = Session { sessionUserID = userID user }
  eRes <- lift $ liftIO $
    makeJWT session jwtSettings (Just (addUTCTime 86400 time))
  case eRes of
    Left  err   -> throwError $ AuthenticationError $ show err
    Right token -> return token

runAction :: Pool Connection -> Maybe Session -> ConcreteAction a -> Handler a
runAction pool mSession action = do
  eRes <- runLift $ runUserAccess pool $ runTime $ runError $
    checkAccess (fmap sessionUserID mSession) action
  case eRes of
    Left  err -> Except.throwError $ toServantErr err
    Right res -> return res
