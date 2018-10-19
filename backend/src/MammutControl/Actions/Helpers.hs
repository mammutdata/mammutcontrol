{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

module MammutControl.Actions.Helpers
  ( module MammutControl.Actions.Helpers
  , module Servant.Server
  , throwError
  ) where

import           GHC.Exts (Proxy#, proxy#)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal')

#ifdef SLOW
import           Control.Concurrent (threadDelay)
import           Control.Monad.Trans (liftIO)
#endif

import           Control.Monad (forM_)
import           Control.Monad.Base
import           Control.Monad.MultiExcept
import qualified Control.Exception as E
import qualified Control.Monad.Except as Except

import           Data.Aeson
import           Data.Time (addUTCTime)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import           System.IO

import           Servant.Auth.Server
import           Servant.Server

import           MammutControl.AccessControl
import           MammutControl.Data.Group
import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Data.Wallet
import           MammutControl.Error

type Action = AccessControlT DataM

type MonadAction m = ( MonadAccessControl m
                     , MonadMultiError MCError m
                     , MonadTime m
                     , MonadTransaction m
                     , MonadGroup m
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
    makeJWT session jwtSettings $ Just $ addUTCTime (7*24*3600) time
  case eRes of
    Left  err   -> throwError $ AuthenticationError $ show err
    Right token -> return token

runAction :: Pool Connection -> Maybe Session -> Action a -> Handler a
runAction pool mSession action = do
#ifdef SLOW
  liftIO $ threadDelay $ 1000000
#endif
  eRes <- liftBase $ E.try $ flip runDataM pool $ withTransaction $
    runAccessControlT action (sessionUserID <$> mSession)
  case eRes of
    Left err -> do
      liftBase $ hPutStrLn stderr $ "IO error when running action: "
                                    ++ show (err :: E.SomeException)
      Except.throwError systemError
    Right (Left errs) -> do
      forM_ errs $ \err ->
        liftBase $ hPutStrLn stderr $ "Error when running action: " ++ show err
      Except.throwError $ toServantErr errs
    Right (Right res) -> return res

newtype JSONWrapper (key :: Symbol) a = JSONWrapper a

instance (KnownSymbol key, ToJSON a) => ToJSON (JSONWrapper key a) where
  toJSON (JSONWrapper xs) =
    let key = T.pack $ symbolVal' (proxy# :: Proxy# key)
    in object [ key .= xs ]
