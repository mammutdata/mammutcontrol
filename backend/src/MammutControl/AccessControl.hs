{-# LANGUAGE UndecidableInstances #-}

module MammutControl.AccessControl
  ( AccessControlT(..)
  , runAccessControlT
  , MonadAccessControl(..)
  ) where

import Control.Monad.Base
import Control.Monad.MultiExcept
import Control.Monad.Reader

import MammutControl.AccessControl.Class
import MammutControl.Data.Group
import MammutControl.Data.Types
import MammutControl.Data.User
import MammutControl.Data.Wallet
import MammutControl.Error

data AccessType
  = NotLoggedIn
  | LoggedIn UserID
  | Passthrough

newtype AccessControlT m a
  = AccessControlT { unAccessControlT :: ReaderT AccessType m a }
  deriving newtype (Applicative, Functor, Monad, MonadTrans)

deriving newtype instance MonadBase IO m => MonadBase IO (AccessControlT m)
deriving newtype instance MonadMultiError MCError m
  => MonadMultiError MCError (AccessControlT m)
deriving newtype instance MonadTime m => MonadTime (AccessControlT m)

instance MonadTransaction m => MonadTransaction (AccessControlT m) where
  withTransaction action = AccessControlT $ do
    atyp <- ask
    lift $ withTransaction $ runReaderT (unAccessControlT action) atyp

runAccessControlT :: AccessControlT m a -> Maybe UserID -> m a
runAccessControlT action muid =
  let atyp = case muid of
        Just uid -> LoggedIn uid
        Nothing  -> NotLoggedIn
  in flip runReaderT atyp $ unAccessControlT action

deny :: MonadMultiError MCError m => Bool -> String -> AccessControlT m a
deny validSession msg = throwError $ AccessDenied validSession msg

notLoggedInError :: MonadMultiError MCError m => AccessControlT m a
notLoggedInError = deny False "must be logged in"

guardSameUserID :: MonadMultiError MCError m => UserID -> String
                -> AccessControlT m ()
guardSameUserID uid err = do
  atyp <- AccessControlT ask
  case atyp of
    Passthrough -> return ()
    LoggedIn uid' | uid == uid' -> return ()
                  | otherwise -> deny True err
    NotLoggedIn -> notLoggedInError

guardUserIDIn :: MonadMultiError MCError m => [UserID] -> String
              -> AccessControlT m ()
guardUserIDIn uids err = do
  atyp <- AccessControlT ask
  case atyp of
    Passthrough -> return ()
    LoggedIn uid | uid `elem` uids -> return ()
                 | otherwise -> deny True err
    NotLoggedIn -> notLoggedInError

--guardLoggedIn :: MonadMultiError MCError m => AccessControlT m ()
--guardLoggedIn = do
--  muid <- AccessControlT ask
--  case muid of
--    Nothing -> notLoggedInError
--    Just _ -> return ()

instance MonadAccessControl (AccessControlT m) where
  skipAccessControl =
    AccessControlT . withReaderT (const Passthrough) . unAccessControlT
  asUser uid =
    AccessControlT . withReaderT (const (LoggedIn uid)) . unAccessControlT

instance (MonadMultiError MCError m, MonadGroup m, MonadWallet m)
    => MonadGroup (AccessControlT m) where
  createGroupNoOwner group = do
    case groupWalletID group of
      Nothing  -> return ()
      Just wid -> do
        users <- lift $ getUsersByWalletID wid
        guardUserIDIn (map userID users) $
          "can't use wallet with ID " ++ show (unWalletID wid)
    lift $ createGroupNoOwner group

  addUserToGroup gid uid = do
    users <- lift $ getUsersByGroupID gid
    unless (null users) $ -- in which case the group is in creation
      guardUserIDIn (map userID users) $
        "can't add user to group with ID " ++ show (unGroupID gid)
    lift $ addUserToGroup gid uid

  removeUserFromGroup gid uid = do
    void $ guardMemberOfGroup gid $
      "can't remove user from group with ID " ++ show (unGroupID gid)
    lift $ removeUserFromGroup gid uid

  getGroupsByUserID uid = do
    guardSameUserID uid $
      "can't get groups for user with ID " ++ show (unUserID uid)
    lift $ getGroupsByUserID uid

  getUsersByGroupID gid = do
    users <- guardMemberOfGroup gid $
      "can't get users of group with ID " ++ show (unGroupID gid)
    return users

  deleteGroup gid = do
    void $ guardMemberOfGroup gid $
      "can't delete group with ID " ++ show (unGroupID gid)
    lift $ deleteGroup gid

guardMemberOfGroup :: (MonadMultiError MCError m, MonadGroup m) => GroupID
                   -> String -> AccessControlT m [User]
guardMemberOfGroup gid msg = do
  users <- lift $ getUsersByGroupID gid
  guardUserIDIn (map userID users) msg
  return users

instance (MonadMultiError MCError m, MonadUser m)
    => MonadUser (AccessControlT m) where
  hashPassword = lift . hashPassword
  createUser   = lift . createUser

  getUser uid  = do
    guardSameUserID uid $ "can't get user with ID " ++ show (unUserID uid)
    lift $ getUser uid

  getUserByEmail email = do
    user <- lift $ getUserByEmail email
    guardSameUserID (userID user) $ "can't get user with email " ++ show email
    return user

  editUserUnvalidated uid fields = do
    guardSameUserID uid $ "can't edit user with ID " ++ show (unUserID uid)
    lift $ editUserUnvalidated uid fields

  deleteUser uid = do
    guardSameUserID uid $ "can't delete user with ID " ++ show (unUserID uid)
    lift $ deleteUser uid

instance (MonadMultiError MCError m, MonadWallet m)
    => MonadWallet (AccessControlT m) where
  createWalletNoOwner = lift . createWalletNoOwner -- needed on user creation

  addUserToWallet wid uid = do
    uids <- lift $ map userID <$> getUsersByWalletID wid
    unless (null uids) $ -- it's being created
      guardUserIDIn uids $
        "can't add user to wallet with ID " ++ show (unWalletID wid)
    lift $ addUserToWallet wid uid

  getWallet wid = do
    users <- lift $ getUsersByWalletID wid
    guardUserIDIn (map userID users) $
      "can't get wallet with ID " ++ show (unWalletID wid)
    lift $ getWallet wid

  getWalletsByUserID uid = do
    guardSameUserID uid $
      "can't get wallets of user with ID " ++ show (unUserID uid)
    lift $ getWalletsByUserID uid

  getUsersByWalletID wid = do
    users <- lift $ getUsersByWalletID wid
    guardUserIDIn (map userID users) $
      "can't get users from wallet with ID " ++ show (unWalletID wid)
    return users
