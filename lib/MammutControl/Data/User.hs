{-# LANGUAGE UndecidableInstances #-}

module MammutControl.Data.User
  ( PasswordHash
  , UserID(..)
  , User'(..)
  , User
  , UserAccess(..)
  , hashPassword
  , validatePassword
  , createUser
  , getUser
  , getUserByEmail
  , editUser
  , deleteUser
  , runUserAccess
  ) where

import           GHC.Generics (Generic)

import           Control.Arrow
import           Control.Eff
import           Control.Eff.Exception (Exc, catchError, throwError)
import           Control.Eff.Lift (Lift, lift)
import           Control.Monad (unless, void, when)
import           Control.Monad.Base
import           Control.Monad.Trans.Control

import           Data.Aeson
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified Crypto.BCrypt as BCrypt

import           Opaleye

import           Servant (FromHttpApiData(..))

import           MammutControl.Data.Types
import           MammutControl.Error

{-
 - Database
 -}

newtype PasswordHash = PasswordHash { unPasswordHash :: BS.ByteString }

type instance ColumnType PasswordHash = PGBytea

instance QueryRunnerColumnDefault PGBytea PasswordHash where
  queryRunnerColumnDefault = fmap PasswordHash queryRunnerColumnDefault

pgPasswordHash :: PasswordHash -> Column (ColumnType PasswordHash)
pgPasswordHash = pgStrictByteString . unPasswordHash

newtype UserID = UserID { unUserID :: Int64 } deriving Eq

type instance ColumnType UserID = PGInt8

instance QueryRunnerColumnDefault PGInt8 UserID where
  queryRunnerColumnDefault = fmap UserID queryRunnerColumnDefault

instance FromHttpApiData UserID where
  parseUrlPiece = fmap UserID . parseUrlPiece

instance FromJSON UserID where
  parseJSON = fmap UserID . parseJSON

instance ToJSON UserID where
  toJSON = toJSON . unUserID

pgUserID :: UserID -> Column (ColumnType UserID)
pgUserID = pgInt8 . unUserID

data User' f = User
  { userID           :: Field f 'Opt UserID
  , userEmail        :: Field f 'Req T.Text
  , userPasswordHash :: Field f 'Req PasswordHash
  } deriving Generic

type User = User' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'Req T.Text) (Field g 'Req T.Text)
                  , Default p (Field f 'Opt UserID) (Field g 'Opt UserID)
                  , Default p (Field f 'Req PasswordHash)
                              (Field g 'Req PasswordHash)
                  ) => Default p (User' f) (User' g)

instance ToJSON User where
  toJSON User{..} = object
    [ "id"    .= userID
    , "email" .= userEmail
    ]

instance FromJSON (User' Maybe) where
  parseJSON = withObject "user" $ \obj -> do
    mEmail <- obj .:? "email"
    return $ emptyUser { userEmail = mEmail }

emptyUser :: User' Maybe
emptyUser = User Nothing Nothing Nothing

pUser :: User' TblCol -> TableColumns (User' WriteCol) (User' Col)
pUser = genericAdaptor

userTable :: Table (User' WriteCol) (User' Col)
userTable = table "users" $ pUser User
  { userID           = tableColumn "id"
  , userEmail        = tableColumn "email"
  , userPasswordHash = tableColumn "password_hash"
  }

userByID :: QueryArr (Column PGInt8) (User' Col)
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

createUser :: (Member (Exc MCError) r, Member UserAccess r)
           => T.Text -> BS.ByteString -> Eff r User
createUser email password = do
  hash <- hashPassword password
  let user = User
        { userID           = Nothing
        , userEmail        = email
        , userPasswordHash = hash
        }
  validateUser $ hoistFields user
  eUser <- send $ CreateUser user
  either throwError return eUser

hashPassword :: (Member (Exc MCError) r, Member UserAccess r)
             => BS.ByteString -> Eff r PasswordHash
hashPassword password = do
  eHash <- send $ HashPassword password
  either throwError return eHash

validatePassword :: (Member (Exc MCError) r, Member UserAccess r)
                 => User -> BS.ByteString -> Eff r ()
validatePassword user password = do
  let hash = unPasswordHash $ userPasswordHash user
  unless (BCrypt.validatePassword hash password) $
    throwError $ AuthenticationError "wrong password"
  unless (BCrypt.hashUsesPolicy passwordHashingPolicy hash) $ do
    hash' <- hashPassword password
    void $ editUser (userID user) emptyUser { userPasswordHash = Just hash' }

getUser :: (Member (Exc MCError) r, Member UserAccess r) => UserID -> Eff r User
getUser uid = do
  eUser <- send $ GetUser uid
  either throwError return eUser

getUserByEmail :: (Member (Exc MCError) r, Member UserAccess r)
               => T.Text -> Eff r User
getUserByEmail email = do
  eUser <- send $ GetUserByEmail email
  either throwError return eUser

editUser :: (Member (Exc MCError) r, Member UserAccess r)
         => UserID -> User' Maybe -> Eff r User
editUser uid fields = do
  validateUser $ fields { userID = Just uid }
  eUser <- send $ EditUser uid fields
  either throwError return eUser

deleteUser :: (Member (Exc MCError) r, Member UserAccess r)
           => UserID -> Eff r ()
deleteUser uid = do
  eRes <- send $ DeleteUser uid
  either throwError return eRes

validateUser :: (Member (Exc MCError) r, Member UserAccess r)
             => User' Maybe -> Eff r ()
validateUser user = do
  case userEmail user of
    Nothing -> return ()
    Just email -> do
      mUser <- fmap Just (getUserByEmail email)
        `catchError` \(_ :: MCError) -> return Nothing

      let emailTaken = case (mUser, userID user) of
            (Just _, Nothing) -> True
            (Just user', Just uid) -> uid /= userID user'
            _ -> False

      when emailTaken $ throwError $ ValidationError "email" "already taken"

{-
 - Effect
 -}

data UserAccess a where
  HashPassword   :: BS.ByteString -> UserAccess (Either MCError PasswordHash)
  CreateUser     :: User' Write -> UserAccess (Either MCError User)
  GetUser        :: UserID -> UserAccess (Either MCError User)
  GetUserByEmail :: T.Text -> UserAccess (Either MCError User)
  EditUser       :: UserID -> User' Maybe -> UserAccess (Either MCError User)
  DeleteUser     :: UserID -> UserAccess (Either MCError ())

runUserAccess :: (MonadBaseControl IO m, SetMember Lift (Lift m) r)
              => Pool Connection -> Eff (UserAccess ': r) a -> Eff r a
runUserAccess pool = handle_relay return $ \action rest -> case action of
  HashPassword password -> do
    mHash <- lift $ liftBase $
      BCrypt.hashPasswordUsingPolicy passwordHashingPolicy password
    rest $ case mHash of
      Nothing -> Left $ InternalError "hashing failed"
      Just hash -> Right $ PasswordHash hash

  CreateUser user -> do
    let user' = User
          { userID           = fmap pgUserID (userID user)
          , userEmail        = pgStrictText (userEmail user)
          , userPasswordHash = pgPasswordHash (userPasswordHash user)
          }

    res <- lift $ withResource pool $ \conn ->
      liftBase $ runInsertManyReturning conn userTable [user'] id

    rest $ case res of
      user'' : _ -> Right user''
      [] -> Left $ ValidationError "user" "could not insert"

  GetUser uid -> do
    res <- lift $ withResource pool $ \conn ->
      liftBase $ runQuery conn $ limit 1 $ userByID <<^ \() -> pgUserID uid

    rest $ case res of
      user : _ -> Right user
      [] -> Left $ ResourceNotFoundError $ "User #" ++ show (unUserID uid)

  GetUserByEmail email -> do
    res <- lift $ withResource pool $ \conn ->
      liftBase $ runQuery conn $
        limit 1 $ userByEmail <<^ \() -> pgStrictText email

    rest $ case res of
      user : _ -> Right user
      [] -> Left $ ResourceNotFoundError $ "User with email=" ++ show email

  EditUser uid fields -> do
    res <- lift $ withResource pool $ \conn ->
      liftBase $ runUpdateReturning conn userTable
        (\user -> User
           { userID = Just $ userID user
           , userEmail =
               maybe (userEmail user) pgStrictText (userEmail fields)
           , userPasswordHash =
               maybe (userPasswordHash user) pgPasswordHash
                     (userPasswordHash fields)
           })
        (\user -> userID user .== pgUserID uid)
        id

    rest $ case res of
      user : _ -> Right user
      [] -> Left $ ResourceNotFoundError $ "User #" ++ show (unUserID uid)

  DeleteUser uid -> do
    res <- lift $ withResource pool $ \conn ->
      liftBase $ runDelete conn userTable $ \user ->
        userID user .== pgUserID uid

    rest $ case res of
      0 -> Left $ ResourceNotFoundError $ "User #" ++ show (unUserID uid)
      _ -> Right ()

passwordHashingPolicy :: BCrypt.HashingPolicy
passwordHashingPolicy = BCrypt.HashingPolicy
  { BCrypt.preferredHashCost      = 10
  , BCrypt.preferredHashAlgorithm = BCrypt.defaultHashAlgorithm
  }
