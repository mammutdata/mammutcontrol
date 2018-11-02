{-# LANGUAGE UndecidableInstances #-}

module MammutControl.Data.Group.Schema
  ( GroupID(..)
  , Group'(..)
  , Group
  , pGroup
  , groupTable
  , GroupMembership'(..)
  , GroupMembership
  , pGroupMembership
  , groupMembershipTable
  , gmsByUserID
  , gmsByGroupID
  , groupByID
  , groupsByUserID
  , usersByGroupID
  ) where

import           Control.Arrow

import           Data.Aeson
import           Data.Functor.Identity
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import           Data.Time (UTCTime)
import qualified Data.Text as T

import           Opaleye hiding (Field)

import           Servant (FromHttpApiData(..))

import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Data.Wallet

newtype GroupID = GroupID { unGroupID :: Int64 } deriving Eq

type instance ColumnType GroupID = PGInt8

deriving newtype instance QueryRunnerColumnDefault PGInt8 GroupID
deriving newtype instance FromHttpApiData GroupID

instance ToJSON GroupID where
  toJSON = toJSON . show . unGroupID

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

gmsByUserID :: QueryArr (Column (ColumnType UserID)) (GroupMembership' Col)
gmsByUserID = proc uid -> do
  gm <- queryTable groupMembershipTable -< ()
  restrict -< gmUserID gm .== uid
  returnA -< gm

gmsByGroupID :: QueryArr (Column (ColumnType GroupID)) (GroupMembership' Col)
gmsByGroupID = proc gid -> do
  gm <- queryTable groupMembershipTable -< ()
  restrict -< gmGroupID gm .== gid
  returnA -< gm

groupByID :: QueryArr (Column (ColumnType GroupID)) (Group' Col)
groupByID = proc gid -> do
  group <- queryTable groupTable -< ()
  restrict -< groupID group .== gid
  returnA -< group

groupsByUserID :: QueryArr (Column (ColumnType UserID)) (Group' Col)
groupsByUserID = proc uid -> do
  gm <- gmsByUserID -< uid
  groupByID -< gmGroupID gm

usersByGroupID :: QueryArr (Column (ColumnType GroupID)) (User' Col)
usersByGroupID = proc gid -> do
  gm <- gmsByGroupID -< gid
  userByID -< gmUserID gm
