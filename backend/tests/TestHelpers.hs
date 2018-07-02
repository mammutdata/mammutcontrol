module TestHelpers
  ( module TestHelpers
  , module Hedgehog
  , module Test.Tasty
  , module Test.Tasty.Hedgehog
  ) where

import           GHC.Int

import           Control.Monad.MultiExcept
import           Control.Monad.State

import           Data.List
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import           Data.Time
import qualified Data.ByteString as BS

import qualified Crypto.BCrypt as BCrypt

import           Hedgehog

import           Test.Tasty
import           Test.Tasty.Hedgehog

import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Error

once :: Property -> Property
once = withTests 1

data FakeDB = FakeDB
  { fdbNextUserID :: Int64
  , fdbUsers      :: [User]
  }

emptyFakeDB :: FakeDB
emptyFakeDB = FakeDB
  { fdbNextUserID = 0
  , fdbUsers      = []
  }

newtype FakeDataT m a
  = FakeDataT { unFakeDataT :: MultiExceptT MCError
                                 (StateT (FakeDB, UTCTime) m) a }
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance Monad m => MonadMultiError MCError (FakeDataT m)
deriving newtype instance Monad m => MonadState (FakeDB, UTCTime) (FakeDataT m)

initTime :: UTCTime
initTime = UTCTime
  { utctDay     = fromGregorian 2018 03 09
  , utctDayTime = secondsToDiffTime 12345
  }

runFakeDataT :: FakeDB -> UTCTime -> FakeDataT m a
             -> m (Either (NonEmpty MCError) a, (FakeDB, UTCTime))
runFakeDataT fdb t action =
  runStateT (runMultiExceptT (unFakeDataT action)) (fdb, t)

runFakeDataT_ :: Monad m => FakeDataT m a -> m (Either (NonEmpty MCError) a)
runFakeDataT_ = fmap fst . runFakeDataT emptyFakeDB initTime

modifyFakeDB :: Monad m => (FakeDB -> (a, FakeDB)) -> FakeDataT m a
modifyFakeDB f = state $ \(fdb, t) -> let (x, fdb') = f fdb in (x, (fdb', t))

setTime :: Monad m =>  UTCTime -> FakeDataT m ()
setTime t = state $ \(fdb, _) -> ((), (fdb, t))

instance Monad m => MonadTime (FakeDataT m) where
  getTime = snd <$> get

hashPasswordForTests :: Int -> BS.ByteString -> PasswordHash
hashPasswordForTests cost pwd =
  let salt = fromMaybe (error "salting") $ BCrypt.genSalt
        (BCrypt.preferredHashAlgorithm passwordHashingPolicy)
        cost "\DC3^\ACK\SOH\202\160\ENQ\EOT\167\145\145\nd{\208\144"
  in PasswordHash $ fromMaybe (error "hashing") $ BCrypt.hashPassword pwd salt

passwordHashingCostForTests :: Int
passwordHashingCostForTests = 5

instance Monad m => MonadUser (FakeDataT m) where
  hashPassword = return . hashPasswordForTests passwordHashingCostForTests

  createUser user = do
    now <- getTime
    uid <- modifyFakeDB $ \fdb ->
      let i = fdbNextUserID fdb in (UserID i, fdb { fdbNextUserID = i + 1 })
    let user' = User
          { userID           = uid
          , userEmail        = userEmail user
          , userName         = userName user
          , userPasswordHash = userPasswordHash user
          , userCreationTime = now
          }
    modifyFakeDB $ \fdb -> ((), fdb { fdbUsers = user' : fdbUsers fdb })
    return user'

  getUser uid = do
    (fdb, _) <- get
    case find ((==uid) . userID) (fdbUsers fdb) of
      Just user -> return user
      Nothing   -> throwError $
        ResourceNotFoundError RTUser $ "user #" ++ show (unUserID uid)

  getUserByEmail email = do
    (fdb, _) <- get
    case find ((==email) . userEmail) (fdbUsers fdb) of
      Just user -> return user
      Nothing   -> throwError $
        ResourceNotFoundError RTUser $ "email=" ++ show email

  editUserUnvalidated uid fields = do
    (fdb, t) <- get
    case partition ((==uid) . userID) (fdbUsers fdb) of
      ([user], users) -> do
        let user' =
              maybe id (\e u -> u { userEmail = e }) (userEmail fields)
              . maybe id (\n u -> u { userName = n }) (userName fields)
              . maybe id (\h u -> u { userPasswordHash = h })
                         (userPasswordHash fields)
              $ user
        put (fdb { fdbUsers = user' : users }, t)
        return user'
      _ -> throwError $
        ResourceNotFoundError RTUser $ "user #" ++ show (unUserID uid)

  deleteUser uid =
    modifyFakeDB $ \fdb ->
      ((), fdb { fdbUsers = filter ((/=uid) . userID) (fdbUsers fdb) })

data StubResult a
  = StubResult a   -- ^ Stub matched, return the given value
  | StubMatched    -- ^ Stub matched, call the actual function
  | StubNotMatched -- ^ Stub not matched, ignore and call the actual function

data Stub m
  = HashPassword (BS.ByteString -> m (StubResult PasswordHash))
  | EditUser (UserID -> User' Maybe -> m (StubResult User))

newtype StubbedT m a
  = StubbedT { unStubbedT :: StateT [Stub m] m a }
  deriving newtype (Functor, Applicative, Monad)

type StubbedDataT m = StubbedT (FakeDataT m)

deriving newtype instance Monad m => MonadState [Stub m] (StubbedT m)
deriving newtype instance MonadMultiError e m => MonadMultiError e (StubbedT m)

instance MonadTrans StubbedT where
  lift = StubbedT . lift

stub :: Monad m => Stub m -> StubbedT m ()
stub s = state $ \stubs -> ((), stubs ++ [s])

instance MonadTime m => MonadTime (StubbedT m) where
  getTime = lift getTime

instance (Monad m, MonadUser m) => MonadUser (StubbedT m) where
  hashPassword pwd = do
    stubs <- get
    case stubs of
      HashPassword f : stubs' -> do
        res <- lift $ f pwd
        case res of
          StubNotMatched -> lift $ hashPassword pwd
          StubMatched -> put stubs' >> lift (hashPassword pwd)
          StubResult x -> put stubs' >> return x
      _ -> lift $ hashPassword pwd

  createUser = lift . createUser
  getUser = lift . getUser
  getUserByEmail = lift . getUserByEmail

  editUserUnvalidated uid fields = do
    stubs <- get
    case stubs of
      EditUser f : stubs' -> do
        res <- lift $ f uid fields
        case res of
          StubNotMatched -> lift $ editUserUnvalidated uid fields
          StubMatched -> put stubs' >> lift (editUserUnvalidated uid fields)
          StubResult x -> put stubs' >> return x
      _ -> lift $ editUserUnvalidated uid fields

  deleteUser = lift . deleteUser

-- | Run an action with stubs. Note that the multiple error behaviour of
-- FakeDataM will be lost when using this function because of the wrapping
-- state monad.
runStubbedDataT :: FakeDB -> UTCTime -> [Stub (FakeDataT m)] -> StubbedDataT m a
                -> m ( Either (NonEmpty MCError)
                              (a, [Stub (FakeDataT m)])
                     , (FakeDB, UTCTime) )
runStubbedDataT fdb t stubs action =
  flip runStateT (fdb, t) $ runMultiExceptT $ unFakeDataT $
    flip runStateT stubs $ unStubbedT action

runStubbedDataT_ :: Monad m => StubbedDataT m a
                 -> m (Either (NonEmpty MCError) a)
runStubbedDataT_ action = do
  eRes <- fst <$> runStubbedDataT emptyFakeDB initTime [] action
  case eRes of
    Left errs -> return $ Left errs
    Right (x, []) -> return $ Right x
    Right _ -> error "some stubs left"
