{-# LANGUAGE UndecidableInstances #-}

module MammutControl.AccessControl
  ( AccessControlT(..)
  , runAccessControlT
  ) where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans

import MammutControl.Data.Types
import MammutControl.Data.User
import MammutControl.Data.Wallet
import MammutControl.Error

newtype AccessControlT m a
  = AccessControlT { unAccessControlT :: ReaderT (Maybe UserID) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

deriving newtype instance MonadBase IO m => MonadBase IO (AccessControlT m)
deriving newtype instance MonadError MCError m
  => MonadError MCError (AccessControlT m)
deriving newtype instance MonadTime m => MonadTime (AccessControlT m)

instance MonadTransaction m => MonadTransaction (AccessControlT m) where
  withTransaction action = AccessControlT $ do
    muid <- ask
    lift $ withTransaction $ runAccessControlT action muid

runAccessControlT :: AccessControlT m a -> Maybe UserID -> m a
runAccessControlT action muid = flip runReaderT muid $ unAccessControlT action

deny :: MonadError MCError m => String -> AccessControlT m a
deny = throwError . AccessDenied

notLoggedInError :: MonadError MCError m => AccessControlT m a
notLoggedInError = deny "must be logged in"

guardSameUserID :: MonadError MCError m => UserID -> String
                -> AccessControlT m ()
guardSameUserID uid err = do
  muid <- AccessControlT ask
  case muid of
    Just uid' | uid == uid' -> return ()
              | otherwise -> deny err
    Nothing -> notLoggedInError

guardUserIDIn :: MonadError MCError m => [UserID] -> String
              -> AccessControlT m ()
guardUserIDIn uids err = do
  muid <- AccessControlT ask
  case muid of
    Just uid | uid `elem` uids -> return ()
             | otherwise -> deny err
    Nothing -> notLoggedInError

guardLoggedIn :: MonadError MCError m => AccessControlT m ()
guardLoggedIn = do
  muid <- AccessControlT ask
  case muid of
    Nothing -> notLoggedInError
    Just _ -> return ()

instance (MonadError MCError m, MonadUser m)
    => MonadUser (AccessControlT m) where
  hashPassword = lift . hashPassword
  createUser   = lift . createUser

  getUser uid  = do
    guardSameUserID uid $ "can't get user with ID " ++ show (unUserID uid)
    lift $ getUser uid

  getUserByEmail = lift . getUserByEmail -- needed to sign in and for validation

  editUserUnvalidated uid fields = do
    guardSameUserID uid $ "can't edit user with ID " ++ show (unUserID uid)
    lift $ editUserUnvalidated uid fields

  deleteUser uid = do
    guardSameUserID uid $ "can't delete user with ID " ++ show (unUserID uid)
    lift $ deleteUser uid

instance (MonadError MCError m, MonadWallet m)
    => MonadWallet (AccessControlT m) where
  createWalletNoOwner = lift . createWalletNoOwner -- needed on user creation

  addUserToWallet wid uid = do
    uids <- lift $ map userID <$> getUsersByWalletID wid
    unless (null uids) $ -- it's being created
      guardUserIDIn uids $
        "can't add user to wallet with ID " ++ show (unWalletID wid)
    lift $ addUserToWallet wid uid

  getWalletsByUserID uid = do
    guardSameUserID uid $
      "can't get wallets of user with ID " ++ show (unUserID uid)
    lift $ getWalletsByUserID uid

  getUsersByWalletID wid = do
    users <- lift $ getUsersByWalletID wid
    guardUserIDIn (map userID users) $
      "can't get users from wallet with ID " ++ show (unWalletID wid)
    return users
