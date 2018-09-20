module MammutControl.Actions.GroupActions
  ( getGroupsAction
  , GroupData(..)
  , createGroupAction
  , getMembersOfGroupAction
  , GroupMembershipData(..)
  , addUserToGroupAction
  , removeUserFromGroupAction
  ) where

import           Data.Aeson
import qualified Data.Text as T

import           Servant

import           MammutControl.AccessControl
import           MammutControl.Actions.Helpers
import           MammutControl.Data.Group
import           MammutControl.Data.User
import           MammutControl.Data.Wallet

getGroupsAction :: MonadAction m => Session -> m (JSONWrapper "groups" [Group])
getGroupsAction = fmap JSONWrapper . getGroupsByUserID . sessionUserID

getGroupAction :: MonadAction m => GroupID -> m (JSONWrapper "group" Group)
getGroupAction = undefined

data GroupData = GroupData T.Text (Maybe T.Text) (Maybe WalletID)

instance FromJSON GroupData where
  parseJSON = withObject "group data" $ \obj -> GroupData
    <$> obj .:  "name"
    <*> obj .:? "description"
    <*> obj .:? "wallet_id"

createGroupAction :: MonadAction m => Session -> GroupData -> m Group
createGroupAction session (GroupData name mDescription mWalletID) = do
  let group = Group
        { groupID           = ()
        , groupName         = name
        , groupDescription  = mDescription
        , groupWalletID     = mWalletID
        , groupCreationTime = ()
        }
  createGroup group (sessionUserID session)

getMembersOfGroupAction :: MonadAction m => GroupID -> m [User]
getMembersOfGroupAction = getUsersByGroupID

newtype GroupMembershipData = GroupMembershipData T.Text

instance FromJSON GroupMembershipData where
  parseJSON = withObject "group membership data" $ \obj ->
    GroupMembershipData <$> obj .: "email"

addUserToGroupAction :: MonadAction m => GroupID -> GroupMembershipData
                     -> m [User]
addUserToGroupAction gid (GroupMembershipData email) = do
  user <- skipAccessControl $ getUserByEmail email
  addUserToGroup gid (userID user)
  getUsersByGroupID gid

removeUserFromGroupAction :: MonadAction m => GroupID -> UserID -> m NoContent
removeUserFromGroupAction gid uid = do
  removeUserFromGroup gid uid
  return NoContent
