{-# LANGUAGE UndecidableInstances #-}

module MammutControl.Data.Replica
  ( ReplicaID(..)
  , Replica'(..)
  , Replica
  , MonadReplica(..)
  , replicaByID
  ) where

import           Prelude hiding (null)

import           Control.Arrow

import           Data.Aeson
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import           Data.Time (UTCTime)
import qualified Data.Text as T

import           Opaleye

import           Servant (FromHttpApiData(..))

import           MammutControl.Data.Group
import           MammutControl.Data.Types
import           MammutControl.Data.Wallet

{-
 - Database
 -}

newtype ReplicaID = ReplicaID { unReplicaID :: Int64 } deriving Eq

type instance ColumnType ReplicaID = PGInt8

deriving newtype instance QueryRunnerColumnDefault PGInt8 ReplicaID
deriving newtype instance FromHttpApiData ReplicaID

instance ToJSON ReplicaID where
  toJSON = toJSON . show . unReplicaID

instance Default Constant ReplicaID (Column PGInt8) where
  def = lmap unReplicaID def

data Replica' f = Replica
  { replicaID           :: Field f 'ReadOnly ReplicaID
  , replicaGroupID      :: Field f 'Required GroupID
  , replicaName         :: Field f 'Required T.Text
  , replicaDescription  :: Field f 'Optional (Maybe T.Text)
  , replicaWalletID     :: Field f 'Optional (Maybe WalletID)
  , replicaCreationTime :: Field f 'ReadOnly UTCTime
  } deriving Generic

type Replica = Replica' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'ReadOnly ReplicaID)
                              (Field g 'ReadOnly ReplicaID)
                  , Default p (Field f 'Required GroupID)
                              (Field g 'Required GroupID)
                  , Default p (Field f 'Required T.Text)
                              (Field g 'Required T.Text)
                  , Default p (Field f 'Optional (Maybe T.Text))
                              (Field g 'Optional (Maybe T.Text))
                  , Default p (Field f 'Optional (Maybe WalletID))
                              (Field g 'Optional (Maybe WalletID))
                  , Default p (Field f 'ReadOnly UTCTime)
                              (Field g 'ReadOnly UTCTime)
                  ) => Default p (Replica' f) (Replica' g)

instance ToJSON Replica where
  toJSON Replica{..} = object $
    [ "id"       .= replicaID
    , "group_id" .= replicaGroupID
    , "name"     .= replicaName
    ] ++ maybe [] (pure . ("description" .=)) replicaDescription
      ++ maybe [] (pure . ("wallet_id"   .=)) replicaWalletID

instance FromJSON (Replica' Maybe) where
  parseJSON = withObject "replica" $ \obj -> do
    mName        <- obj .:? "name"
    mDescription <- obj .:? "description"
    mWalletID    <- obj .:? "wallet_id"
    return emptyReplica
      { replicaName        = mName
      , replicaDescription = mDescription
      , replicaWalletID    = mWalletID
      }

emptyReplica :: Replica' Maybe
emptyReplica = Replica Nothing Nothing Nothing Nothing Nothing Nothing

pReplica :: Replica' TblCol -> TableColumns (Replica' WriteCol) (Replica' Col)
pReplica = genericAdaptor

replicaTable :: Table (Replica' WriteCol) (Replica' Col)
replicaTable = table "replicas" $ pReplica Replica
  { replicaID           = tableColumn "id"
  , replicaGroupID      = tableColumn "group_id"
  , replicaName         = tableColumn "name"
  , replicaDescription  = tableColumn "description"
  , replicaWalletID     = tableColumn "wallet_id"
  , replicaCreationTime = tableColumn "creation_time"
  }

replicaByID :: QueryArr (Column (ColumnType ReplicaID)) (Replica' Col)
replicaByID = proc uid -> do
  replica <- queryTable replicaTable -< ()
  restrict -< replicaID replica .== uid
  returnA -< replica

{-
 - Logic
 -}

--validateReplica :: (MonadError MCError m, MonadReplica m) => Replica' Maybe
--                -> m ()
--validateReplica _ = undefined

{-
 - Effect
 -}

class MonadReplica m where
  placeholder :: m ()
