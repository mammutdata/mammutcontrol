{-# LANGUAGE UndecidableInstances #-}

module MammutControl.Data.Group
  ( GroupID(..)
  , Group'(..)
  , Group
  , MonadGroup(..)
  , createGroup
  ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.MultiExcept

import           Data.Aeson
import           Data.Functor.Identity
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import           Data.Time (UTCTime)
import qualified Data.Text as T

import           Opaleye

import           Servant (FromHttpApiData(..))

import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Data.Wallet
import           MammutControl.Error

{-
 - Database
 -}

newtype GroupID = GroupID { unGroupID :: Int64 } deriving Eq

type instance ColumnType GroupID = PGInt8

deriving newtype instance QueryRunnerColumnDefault PGInt8 GroupID
deriving newtype instance FromHttpApiData GroupID
deriving newtype instance FromJSON GroupID
deriving newtype instance ToJSON GroupID

instance Default Constant GroupID (Column PGInt8) where
  def = lmap unGroupID def

data Group' f = Group
  { groupID           :: Field f 'ReadOnly GroupID
  , groupName         :: Field f 'Required T.Text
  , groupDescription  :: Field f 'Optional (Maybe T.Text)
  , groupWalletID     :: Field f 'Optional (Maybe WalletID)
  , groupCreationTime :: Field f 'ReadOnly UTCTime
  } deriving Generic

type Group = Group' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'ReadOnly GroupID)
                              (Field g 'ReadOnly GroupID)
                  , Default p (Field f 'Required T.Text)
                              (Field g 'Required T.Text)
                  , Default p (Field f 'Optional (Maybe T.Text))
                              (Field g 'Optional (Maybe T.Text))
                  , Default p (Field f 'Optional (Maybe WalletID))
                              (Field g 'Optional (Maybe WalletID))
                  , Default p (Field f 'ReadOnly UTCTime)
                              (Field g 'ReadOnly UTCTime)
                  ) => Default p (Group' f) (Group' g)

instance ToJSON Group where
  toJSON Group{..} = object $
    [ "id"   .= groupID
    , "name" .= groupName
    ] ++ maybe [] (pure . ("description" .=)) groupDescription
      ++ maybe [] (pure . ("wallet_id"   .=)) groupWalletID

pGroup :: Group' TblCol -> TableColumns (Group' WriteCol) (Group' Col)
pGroup = genericAdaptor

groupTable :: Table (Group' WriteCol) (Group' Col)
groupTable = table "active_groups" $ pGroup Group
  { groupID           = tableColumn "id"
  , groupName         = tableColumn "name"
  , groupDescription  = tableColumn "description"
  , groupWalletID     = tableColumn "wallet_id"
  , groupCreationTime = tableColumn "creation_time"
  }

data GroupMembership' f = GroupMembership
  { gmUserID  :: Field f 'Required UserID
  , gmGroupID :: Field f 'Required GroupID
  } deriving Generic

type GroupMembership = GroupMembership' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'Required UserID)
                              (Field g 'Required UserID)
                  , Default p (Field f 'Required GroupID)
                              (Field g 'Required GroupID)
                  ) => Default p (GroupMembership' f) (GroupMembership' g)

pGroupMembership :: GroupMembership' TblCol
                 -> TableColumns (GroupMembership' WriteCol)
                                 (GroupMembership' Col)
pGroupMembership = genericAdaptor

groupMembershipTable :: Table (GroupMembership' WriteCol)
                              (GroupMembership' Col)
groupMembershipTable =
  table "group_memberships" $ pGroupMembership GroupMembership
    { gmUserID       = tableColumn "user_id"
    , gmGroupID      = tableColumn "group_id"
    }

gmByUserID :: QueryArr (Column (ColumnType UserID)) (GroupMembership' Col)
gmByUserID = proc uid -> do
  gm <- queryTable groupMembershipTable -< ()
  restrict -< gmUserID gm .== uid
  returnA -< gm

gmByGroupID :: QueryArr (Column (ColumnType GroupID)) (GroupMembership' Col)
gmByGroupID = proc gid -> do
  gm <- queryTable groupMembershipTable -< ()
  restrict -< gmGroupID gm .== gid
  returnA -< gm

--groupByID :: QueryArr (Column (ColumnType GroupID)) (Group' Col)
--groupByID = proc gid -> do
--  group <- queryTable groupTable -< ()
--  restrict -< groupID group .== gid
--  returnA -< group

groupsByUserID :: QueryArr (Column (ColumnType UserID)) (Group' Col)
groupsByUserID = proc uid -> do
  group <- queryTable groupTable -< ()
  restrictExists gmByUserID -< uid
  returnA -< group

usersByGroupID :: QueryArr (Column (ColumnType GroupID)) (User' Col)
usersByGroupID = proc gid -> do
  gm <- gmByGroupID -< gid
  userByID -< gmUserID gm

{-
 - Logic
 -}

createGroup :: (MonadMultiError MCError m, MonadGroup m, MonadTransaction m)
            => Group' Write -> UserID -> m Group
createGroup group uid = withTransaction $ do
  group' <- createGroupNoOwner group
  addUserToGroup (groupID group') uid
  return group'

class MonadGroup m where
  createGroupNoOwner  :: Group' Write -> m Group
  addUserToGroup      :: GroupID -> UserID -> m ()
  removeUserFromGroup :: GroupID -> UserID -> m ()
  getGroupsByUserID   :: UserID -> m [Group]
  getUsersByGroupID   :: GroupID -> m [User]
  deleteGroup         :: GroupID -> m ()

instance MonadGroup DataM where
  createGroupNoOwner  = createGroupNoOwnerDataM
  addUserToGroup      = addUserToGroupDataM
  removeUserFromGroup = removeUserFromGroupDataM
  getGroupsByUserID   = getGroupsByUserIDDataM
  getUsersByGroupID   = getUsersByGroupIDDataM
  deleteGroup         = deleteGroupDataM

createGroupNoOwnerDataM :: Group' Write -> DataM Group
createGroupNoOwnerDataM group = do
  res <- withConn $ \conn ->
    runInsertManyReturning conn groupTable [hoistFields group] id

  case res of
    wallet' : _ -> return wallet'
    [] -> throwError $ ValidationError Nothing "could not create group"

addUserToGroupDataM :: GroupID -> UserID -> DataM ()
addUserToGroupDataM gid uid = do
  let membership = hoistFields (GroupMembership uid gid :: GroupMembership)
  res <- withConn $ \conn ->
    runInsertMany conn groupMembershipTable [membership]

  if res > 0
    then return ()
    else throwError $ ValidationError Nothing "could not add user to group"

-- This could be done in one request but Opaleye doesn't seem to support
-- DELETE FROM ... WHERE EXISTS (SELECT ...)
removeUserFromGroupDataM :: GroupID -> UserID -> DataM ()
removeUserFromGroupDataM gid uid = do
  res <- withConn $ \conn ->
    runQuery conn $ countRows $ proc () -> do
      user <- usersByGroupID -< constant gid
      restrict -< userID user ./= constant uid
      returnA -< user

  when (res == [0 :: Int64]) $
    throwError $ ConstraintCheckError ConstraintGroupHasAtLeastOneMember

  void $ withConn $ \conn ->
    runDelete conn groupMembershipTable $ \gm ->
      gmGroupID gm .== constant gid
      .&& gmUserID gm .== constant uid

getGroupsByUserIDDataM :: UserID -> DataM [Group]
getGroupsByUserIDDataM uid = withConn $ \conn ->
  runQuery conn $ groupsByUserID <<^ \() -> constant uid

getUsersByGroupIDDataM :: GroupID -> DataM [User]
getUsersByGroupIDDataM gid = withConn $ \conn ->
  runQuery conn $ usersByGroupID <<^ \() -> constant gid

deleteGroupDataM :: GroupID -> DataM ()
deleteGroupDataM gid = do
  withConn $ \conn -> do
    void $ runDelete conn groupMembershipTable $ \gm ->
      gmGroupID gm .== constant gid
    void $ runDelete conn groupTable $ \group ->
      groupID group .== constant gid
