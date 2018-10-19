{-# LANGUAGE UndecidableInstances #-}

module TestHelpers
  ( module TestHelpers
  , module Hedgehog
  , module Test.Tasty
  , module Test.Tasty.Hedgehog
  ) where

import           GHC.Int

import           Control.Monad.Base
import           Control.Monad.MultiExcept
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List hiding (group)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Time
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

import           Crypto.JOSE.JWK (fromOctets)
import qualified Crypto.BCrypt as BCrypt

import           Servant.Auth.Server (JWTSettings, defaultJWTSettings)

import           Hedgehog hiding (Group(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Tasty
import           Test.Tasty.Hedgehog

import           MammutControl.AccessControl
import           MammutControl.Actions.UserActions
import           MammutControl.Data.Group
import           MammutControl.Data.Replica
import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Data.Wallet
import           MammutControl.Error

once :: Property -> Property
once = withTests 1

data FakeDB = FakeDB
  { fdbNextUserID    :: Int64
  , fdbUsers         :: [User]
  , fdbNextGroupID   :: Int64
  , fdbGroups        :: [(Group, [UserID])]
  , fdbNextWalletID  :: Int64
  , fdbWallets       :: [(Wallet, [UserID])]
  , fdbNextReplicaID :: Int64
  , fdbReplicas      :: [Replica]
  }

emptyFakeDB :: FakeDB
emptyFakeDB = FakeDB
  { fdbNextUserID    = 1
  , fdbUsers         = []
  , fdbNextGroupID   = 1
  , fdbGroups        = []
  , fdbNextWalletID  = 1
  , fdbWallets       = []
  , fdbNextReplicaID = 1
  , fdbReplicas      = []
  }

newtype FakeDataT m a
  = FakeDataT { unFakeDataT :: MultiExceptT MCError
                                 (StateT (FakeDB, UTCTime) m) a }
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance Monad m => MonadMultiError MCError (FakeDataT m)
deriving newtype instance Monad m => MonadState (FakeDB, UTCTime) (FakeDataT m)
deriving newtype instance MonadBase IO m => MonadBase IO (FakeDataT m)

instance MonadTest m => MonadTest (FakeDataT m) where
  liftTest = FakeDataT . lift . lift . liftTest

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

hashPasswordForTests :: Int -> BS8.ByteString -> PasswordHash
hashPasswordForTests cost pwd =
  let salt = fromMaybe (error "salting") $ BCrypt.genSalt
        (BCrypt.preferredHashAlgorithm passwordHashingPolicy) cost
        "randrandrandrand"
  in PasswordHash $ fromMaybe (error "hashing") $ BCrypt.hashPassword pwd salt

passwordHashingCostForTests :: Int
passwordHashingCostForTests = 5

instance Monad m => MonadTransaction (FakeDataT m) where
  withTransaction = id

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

instance Monad m => MonadGroup (FakeDataT m) where
  getGroup gid = do
    (fdb, _) <- get
    case find ((==gid) . groupID) (map fst (fdbGroups fdb)) of
      Just group -> return group
      Nothing -> throwError $
        ResourceNotFoundError RTGroup $ "group #" ++ show (unGroupID gid)

  createGroupNoOwner group = do
    (_, t) <- get
    gid <- modifyFakeDB $ \fdb ->
      let i = fdbNextGroupID fdb in (GroupID i, fdb { fdbNextGroupID = i + 1 })
    let group' = Group
          { groupID           = gid
          , groupName         = groupName group
          , groupDescription  = groupDescription group
          , groupWalletID     = groupWalletID group
          , groupCreationTime = t
          }
    modifyFakeDB $ \fdb ->
      ((), fdb { fdbGroups = (group', []) : fdbGroups fdb })
    return group'

  addUserToGroup gid uid = modifyFakeDB $ \fdb ->
    case partition ((==gid) . groupID . fst) (fdbGroups fdb) of
      ([(group, uids)], groups) ->
        let groups' = (group, uid : uids) : groups
        in ((), fdb { fdbGroups = groups' })
      _ -> ((), fdb)

  removeUserFromGroup gid uid = do
    (fdb, t) <- get
    case partition ((==gid) . groupID . fst) (fdbGroups fdb) of
      ([(group, uids)], groups) -> do
        let uids'   = filter (/=uid) uids
            groups' = (group, uids') : groups
        when (null uids') $
          throwError $ ConstraintCheckError ConstraintGroupHasAtLeastOneMember
        put (fdb { fdbGroups = groups' }, t)
      _ -> throwError $
        ConstraintCheckError ConstraintGroupHasAtLeastOneMember

  getGroupsByUserID uid = do
    (fdb, _) <- get
    let groups = filter ((uid `elem`) . snd) (fdbGroups fdb)
    return $ flip map groups $ \(group, uids) ->
      let replicaCount = fromIntegral . length $
            filter ((==groupID group) . replicaGroupID) (fdbReplicas fdb)
          userCount = fromIntegral $ length uids
      in GroupSummary group userCount replicaCount

  getUsersByGroupID gid = do
    (fdb, _) <- get
    return $ case find ((== gid) . groupID . fst) (fdbGroups fdb) of
      Nothing -> []
      Just (_, uids) -> filter ((`elem` uids) . userID) (fdbUsers fdb)

  deleteGroup gid = modifyFakeDB $ \fdb ->
    let groups = filter ((/=gid) . groupID . fst) (fdbGroups fdb)
    in ((), fdb { fdbGroups = groups })

instance Monad m => MonadWallet (FakeDataT m) where
  createWalletNoOwner wallet = do
    (_, t) <- get
    wid <- modifyFakeDB $ \fdb ->
      let i = fdbNextWalletID fdb
      in (WalletID i, fdb { fdbNextWalletID = i + 1 })
    let wallet' = Wallet
          { walletID           = wid
          , walletName         = walletName wallet
          , walletDescription  = walletDescription wallet
          , walletCredits      = 0
          , walletCreationTime = t
          }
    modifyFakeDB $ \fdb ->
      ((), fdb { fdbWallets = (wallet', []) : fdbWallets fdb })
    return wallet'

  addUserToWallet gid uid = modifyFakeDB $ \fdb ->
    case partition ((==gid) . walletID . fst) (fdbWallets fdb) of
      ([(wallet, uids)], wallets) ->
        let wallets' = (wallet, uid : uids) : wallets
        in ((), fdb { fdbWallets = wallets' })
      _ -> ((), fdb)

  getWallet wid = do
    (fdb, _) <- get
    case find ((==wid) . walletID . fst) (fdbWallets fdb) of
      Just (wallet, _) -> return wallet
      Nothing -> throwError $
        ResourceNotFoundError RTWallet $ "wallet #" ++ show (unWalletID wid)

  getUsersByWalletID gid = do
    (fdb, _) <- get
    return $ case find ((== gid) . walletID . fst) (fdbWallets fdb) of
      Nothing -> []
      Just (_, uids) -> filter ((`elem` uids) . userID) (fdbUsers fdb)

  getWalletsByUserID uid = do
    (fdb, _) <- get
    return $ map fst $ filter (elem uid . snd) $ fdbWallets fdb

data StubResult a
  = StubResult a   -- ^ Stub matched, return the given value
  | StubMatched    -- ^ Stub matched, call the actual function
  | StubNotMatched -- ^ Stub not matched, ignore and call the actual function

data Stub m
  = HashPassword (BS8.ByteString -> m (StubResult PasswordHash))
  | EditUser (UserID -> User' Maybe -> m (StubResult User))

newtype StubbedT m a
  = StubbedT { unStubbedT :: StateT [Stub m] m a }
  deriving newtype (Functor, Applicative, Monad)

type StubbedDataT m = StubbedT (FakeDataT m)

deriving newtype instance Monad m => MonadState [Stub m] (StubbedT m)
deriving newtype instance MonadMultiError e m => MonadMultiError e (StubbedT m)
deriving newtype instance MonadBase IO m => MonadBase IO (StubbedT m)

instance MonadTrans StubbedT where
  lift = StubbedT . lift

stub :: Monad m => Stub m -> StubbedT m ()
stub s = state $ \stubs -> ((), stubs ++ [s])

instance MonadTransaction m => MonadTransaction (StubbedT m) where
  withTransaction = id

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

instance (Monad m, MonadGroup m) => MonadGroup (StubbedT m) where
  getGroup = lift . getGroup
  createGroupNoOwner = lift . createGroupNoOwner
  addUserToGroup gid uid = lift $ addUserToGroup gid uid
  removeUserFromGroup gid uid = lift $ removeUserFromGroup gid uid
  getGroupsByUserID = lift . getGroupsByUserID
  getUsersByGroupID = lift . getUsersByGroupID
  deleteGroup = lift . deleteGroup

instance (Monad m, MonadWallet m) => MonadWallet (StubbedT m) where
  createWalletNoOwner = lift . createWalletNoOwner
  addUserToWallet wid uid = lift $ addUserToWallet wid uid
  getWallet = lift . getWallet
  getWalletsByUserID = lift . getWalletsByUserID
  getUsersByWalletID = lift . getUsersByWalletID

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

dummyJWTSettings :: JWTSettings
dummyJWTSettings = defaultJWTSettings $ fromOctets $ BS8.replicate 64 'a'

ignoreAccessControl :: AccessControlT m a -> m a
ignoreAccessControl action =
  runAccessControlT (skipAccessControl action) Nothing

runAction :: Maybe UserID -> AccessControlT m a -> m a
runAction = flip runAccessControlT

runActionWithUser :: MonadBase IO m
                  => (User -> AccessControlT (StubbedDataT m) a)
                  -> StubbedDataT m a
runActionWithUser f = do
  (fdb, _) <- lift get
  let i = fdbNextUserID fdb
  LoginInfo user _ <- runAction Nothing $
    createUserAction dummyJWTSettings $ UserCreationData
      ("user" <> T.pack (show i) <> "@email.tld") "Random user" "secret"
  runAction (Just (userID user)) (f user)

nameGen :: Gen T.Text
nameGen = Gen.text (Range.linear 1 10) Gen.alphaNum

descriptionGen :: Gen T.Text
descriptionGen = Gen.text (Range.linear 1 100) Gen.alphaNum

assertAccessDenied :: MonadTest m => Either (NonEmpty MCError) a -> m ()
assertAccessDenied = \case
  Left (err :| errs) -> do
    case err of
      AccessDenied Valid _ -> success
      _ -> annotateShow err >> failure
    annotateShow errs
    assert $ null errs
  Right _ -> failure
