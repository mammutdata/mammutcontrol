module MammutControl.Data.Group
  ( GroupID(..)
  , Group'(..)
  , Group
  , GroupSummary(..)
  , MonadGroup(..)
  , createGroup
  ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.MultiExcept

import           Data.Aeson
import           Data.Profunctor
import           Data.Profunctor.Product
import qualified Data.Text as T

import           Opaleye

import           MammutControl.Data.Group.Schema
import           MammutControl.Data.Replica
import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Error

createGroup :: (MonadMultiError MCError m, MonadGroup m, MonadTransaction m)
            => Group' Write -> UserID -> m Group
createGroup group uid = withTransaction $ do
  validateGroup $ hoistFields group
  group' <- createGroupNoOwner group
  addUserToGroup (groupID group') uid
  return group'

validateGroup :: (MonadMultiError MCError m, MonadGroup m)
              => Group' Maybe -> m ()
validateGroup group = sequenceAll_
  [ when (maybe False T.null (groupName group)) $
      throwError $ ValidationError (Just ("name", CantBeEmpty))
                                   "name can't be empty"
  , when (maybe False T.null (join (groupDescription group))) $
      throwError $ ValidationError (Just ("description", CantBeEmpty))
                                   "description can't be empty"
  ]

data GroupSummary = GroupSummary
  { groupSummaryGroup        :: Group
  , groupSummaryUserCount    :: Int64
  , groupSummaryReplicaCount :: Int64
  } deriving Generic

instance ToJSON GroupSummary where
  toJSON GroupSummary{..} =
    let Object obj = toJSON groupSummaryGroup
        Object obj' = object
          [ "user_count"    .= groupSummaryUserCount
          , "replica_count" .= groupSummaryReplicaCount
          ]
    in Object $ obj <> obj'

class MonadGroup m where
  getGroup            :: GroupID -> m Group
  createGroupNoOwner  :: Group' Write -> m Group
  addUserToGroup      :: GroupID -> UserID -> m ()
  removeUserFromGroup :: GroupID -> UserID -> m ()
  getGroupsByUserID   :: UserID -> m [GroupSummary]
  getUsersByGroupID   :: GroupID -> m [User]
  deleteGroup         :: GroupID -> m ()

instance MonadGroup DataM where
  getGroup            = getGroupDataM
  createGroupNoOwner  = createGroupNoOwnerDataM
  addUserToGroup      = addUserToGroupDataM
  removeUserFromGroup = removeUserFromGroupDataM
  getGroupsByUserID   = getGroupsByUserIDDataM
  getUsersByGroupID   = getUsersByGroupIDDataM
  deleteGroup         = deleteGroupDataM

getGroupDataM :: GroupID -> DataM Group
getGroupDataM gid = do
  res <- withConn $ \conn -> do
    runQuery conn $ limit 1 $ groupByID <<^ \() -> constant gid
  case res of
    group : _ -> return group
    [] -> throwError $ ResourceNotFoundError RTGroup $
            "group #" ++ show (unGroupID gid)

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

groupAggregator :: Aggregator (Group' Col) (Group' Col)
groupAggregator = Group
  <$> lmap groupID           groupBy
  <*> lmap groupName         groupBy
  <*> lmap groupDescription  groupBy
  <*> lmap groupWalletID     groupBy
  <*> lmap groupCreationTime groupBy

getGroupsByUserIDDataM :: UserID -> DataM [GroupSummary]
getGroupsByUserIDDataM uid = withConn $ \conn -> do
  let innerQuery = leftJoinF
        (\g gm -> (g, toNullable (gmUserID gm))) (\g -> (g, Opaleye.null))
        (\g gm -> groupID g .== gmGroupID gm)
        (groupsByUserID <<^ \() -> constant uid) (queryTable groupMembershipTable)
      selectQuery = leftJoinF
        (\(g, mgmuid) r -> (g, mgmuid, toNullable (replicaID r)))
        (\(g, mgmuid) -> (g, mgmuid, Opaleye.null))
        (\(g,_) r -> groupID g .== replicaGroupID r)
        innerQuery (queryTable replicaTable)
      agg = ( groupAggregator
            , distinctAggregator count
            , distinctAggregator count )
      query = aggregate (p3 agg) selectQuery
  map (\(g,uc,rc) -> GroupSummary g uc rc) <$> runQuery conn query

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
