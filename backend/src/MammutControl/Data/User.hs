{-# LANGUAGE UndecidableInstances #-}

module MammutControl.Data.User
  ( PasswordHash(..)
  , UserID(..)
  , User'(..)
  , User
  , MonadUser(..)
  , emptyUser
  , userByID
  , createUserFromData
  , validatePassword
  , validateUser
  , editUser
  , passwordHashingPolicy
  ) where

import           Prelude hiding (null)

import           Control.Arrow
import           Control.Monad (unless, void, when)
import           Control.Monad.Base
import           Control.Monad.MultiExcept ( MonadMultiError, catchErrors
                                           , sequenceAll_, throwError )

import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import           Data.Time (UTCTime)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified Crypto.BCrypt as BCrypt

import           Opaleye

import           MammutControl.AccessControl.Class
import           MammutControl.Data.Types
import           MammutControl.Error

{-
 - Database
 -}

newtype PasswordHash
  = PasswordHash { unPasswordHash :: BS.ByteString }
  deriving (Eq, Show)

type instance ColumnType PasswordHash = PGBytea

deriving newtype instance QueryRunnerColumnDefault PGBytea PasswordHash

instance Default Constant PasswordHash (Column PGBytea) where
  def = lmap unPasswordHash def

data User' f = User
  { userID           :: Field f 'ReadOnly UserID
  , userEmail        :: Field f 'Required T.Text
  , userName         :: Field f 'Required T.Text
  , userPasswordHash :: Field f 'Required PasswordHash
  , userCreationTime :: Field f 'ReadOnly UTCTime
  } deriving Generic

type User = User' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'ReadOnly UserID)
                              (Field g 'ReadOnly UserID)
                  , Default p (Field f 'Required T.Text)
                              (Field g 'Required T.Text)
                  , Default p (Field f 'Required PasswordHash)
                              (Field g 'Required PasswordHash)
                  , Default p (Field f 'ReadOnly UTCTime)
                              (Field g 'ReadOnly UTCTime)
                  ) => Default p (User' f) (User' g)

instance ToJSON User where
  toJSON User{..} = object
    [ "id"    .= userID
    , "email" .= userEmail -- FIXME: personal info
    , "name"  .= userName
    ]

instance FromJSON (User' Maybe) where
  parseJSON = withObject "user" $ \obj -> do
    mEmail <- obj .:? "email"
    mName  <- obj .:? "name"
    return emptyUser { userEmail = mEmail, userName = mName }

emptyUser :: User' Maybe
emptyUser = User Nothing Nothing Nothing Nothing Nothing

pUser :: User' TblCol -> TableColumns (User' WriteCol) (User' Col)
pUser = genericAdaptor

userTable :: Table (User' WriteCol) (User' Col)
userTable = table "active_users" $ pUser User
  { userID           = tableColumn "id"
  , userEmail        = tableColumn "email"
  , userName         = tableColumn "name"
  , userPasswordHash = tableColumn "password_hash"
  , userCreationTime = tableColumn "creation_time"
  }

userByID :: QueryArr (Column (ColumnType UserID)) (User' Col)
userByID = proc uid -> do
  user <- queryTable userTable -< ()
  restrict -< userID user .== uid
  returnA -< user

userByEmail :: QueryArr (Column PGText) (User' Col)
userByEmail = proc email -> do
  user <- queryTable userTable -< ()
  restrict -< userEmail user .== email
  returnA -< user

{-
 - Logic
 -}

createUserFromData :: ( MonadAccessControl m, MonadMultiError MCError m
                      , MonadTime m, MonadUser m )
                   => T.Text -> T.Text -> BS.ByteString -> m User
createUserFromData email name password = do
  hash <- hashPassword password
  let user = User
        { userID           = ()
        , userEmail        = email
        , userName         = name
        , userPasswordHash = hash
        , userCreationTime = ()
        }
  sequenceAll_
    [ validateUser $ hoistFields user
    , when (BS.null password) $
        throwError $ ValidationError (Just ("password", CantBeEmpty))
                                     "password can't be empty"
    ]
  createUser user

validatePassword :: (MonadMultiError MCError m, MonadUser m) => User
                 -> BS.ByteString -> m ()
validatePassword user password = do
  let hash = unPasswordHash $ userPasswordHash user
  unless (BCrypt.validatePassword hash password) $
    throwError $ AuthenticationError "wrong password"
  unless (BCrypt.hashUsesPolicy passwordHashingPolicy hash) $ do
    hash' <- hashPassword password
    void $ editUserUnvalidated (userID user) $
      emptyUser { userPasswordHash = Just hash' }

editUser :: (MonadAccessControl m, MonadMultiError MCError m, MonadUser m)
         => UserID -> User' Maybe -> m User
editUser uid fields = do
  validateUser $ fields { userID = Just uid }
  editUserUnvalidated uid fields

validateUser :: ( MonadAccessControl m, MonadMultiError MCError m
                , MonadUser m ) => User' Maybe -> m ()
validateUser user = do
  emailTaken <- case userEmail user of
    Nothing -> return False
    Just email -> do
      mUser <- skipAccessControl (fmap Just (getUserByEmail email))
        `catchErrors` \(_ :: NonEmpty MCError) -> return Nothing

      return $ case (mUser, userID user) of
        (Just _, Nothing) -> True
        (Just user', Just uid) -> uid /= userID user'
        _ -> False

  sequenceAll_
    [ when (maybe False T.null (userEmail user)) $
        throwError $ ValidationError (Just ("email", CantBeEmpty))
                                     "email can't be empty"
    , when (maybe False T.null (userName user)) $
       throwError $ ValidationError (Just ("name", CantBeEmpty))
                                    "name can't be empty"
    , when emailTaken $
       throwError $ ValidationError (Just ("email", AlreadyTaken))
                                    "email already taken"
    ]

{-
 - Effect
 -}

class MonadUser m where
  hashPassword        :: BS.ByteString -> m PasswordHash
  createUser          :: User' Write -> m User
  getUser             :: UserID -> m User
  getUserByEmail      :: T.Text -> m User
  editUserUnvalidated :: UserID -> User' Maybe -> m User
  deleteUser          :: UserID -> m ()

instance MonadUser DataM where
  hashPassword        = hashPasswordDataM
  createUser          = createUserDataM
  getUser             = getUserDataM
  getUserByEmail      = getUserByEmailDataM
  editUserUnvalidated = editUserUnvalidatedDataM
  deleteUser          = deleteUserDataM

hashPasswordDataM :: BS.ByteString -> DataM PasswordHash
hashPasswordDataM password = do
  mHash <- liftBase $
    BCrypt.hashPasswordUsingPolicy passwordHashingPolicy password
  case mHash of
    Nothing -> throwError $ InternalError "hashing failed"
    Just hash -> return $ PasswordHash hash

createUserDataM :: User' Write -> DataM User
createUserDataM user = do
  res <- withConn $ \conn ->
    runInsertManyReturning conn userTable [hoistFields user] id
  case res of
    user' : _ -> return user'
    [] -> throwError $ InternalError "could not create user"

getUserDataM :: UserID -> DataM User
getUserDataM uid = do
  res <- withConn $ \conn ->
    runQuery conn $ limit 1 $ userByID <<^ \() -> constant uid
  case res of
    user : _ -> return user
    [] -> throwError $ ResourceNotFoundError RTUser $
            "user #" ++ show (unUserID uid)

getUserByEmailDataM :: T.Text -> DataM User
getUserByEmailDataM email = do
  res <- withConn $ \conn ->
    runQuery conn $ limit 1 $ userByEmail <<^ \() -> constant email
  case res of
    user : _ -> return user
    [] -> throwError $ ResourceNotFoundError RTUser $
            "user with email=" ++ show email

editUserUnvalidatedDataM :: UserID -> User' Maybe -> DataM User
editUserUnvalidatedDataM uid fields = do
  res <- withConn $ \conn ->
    runUpdateReturning conn userTable
      (\user -> User
         { userID = Nothing
         , userEmail =
             maybe (userEmail user) constant (userEmail fields)
         , userName =
             maybe (userName user) constant (userName fields)
         , userPasswordHash =
             maybe (userPasswordHash user) constant (userPasswordHash fields)
         , userCreationTime = Nothing
         })
      (\user -> userID user .== constant uid)
      id

  case res of
    user : _ -> return user
    [] -> throwError $ ResourceNotFoundError RTUser $
            "user #" ++ show (unUserID uid)

deleteUserDataM :: UserID -> DataM ()
deleteUserDataM uid = do
  void $ withConn $ \conn ->
    runDelete conn userTable $ \user -> userID user .== constant uid

passwordHashingPolicy :: BCrypt.HashingPolicy
passwordHashingPolicy = BCrypt.HashingPolicy
  { BCrypt.preferredHashCost      = 10
  , BCrypt.preferredHashAlgorithm = BCrypt.defaultHashAlgorithm
  }
