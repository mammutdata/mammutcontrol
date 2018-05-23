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
import           Control.Monad (unless, void, when)
import           Control.Monad.Base
import           Control.Monad.Except (MonadError, catchError, throwError)
import           Control.Monad.Trans.Control

import           Data.Aeson
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import           Data.Proxy
import           Data.Time (UTCTime)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified Crypto.BCrypt as BCrypt

import           Opaleye

import           Servant (FromHttpApiData(..))

import           MammutControl.Data.Group
import           MammutControl.Data.Types
import           MammutControl.Data.Wallet
import           MammutControl.Error

{-
 - Database
 -}

newtype ReplicaID = ReplicaID { unReplicaID :: Int64 } deriving Eq

type instance ColumnType ReplicaID = PGInt8

deriving newtype instance QueryRunnerColumnDefault PGInt8 ReplicaID
deriving newtype instance FromHttpApiData ReplicaID
deriving newtype instance FromJSON ReplicaID
deriving newtype instance ToJSON ReplicaID

instance Default Constant ReplicaID (Column PGInt8) where
  def = lmap unReplicaID def

data Replica' f = Replica
  { replicaID           :: Field f 'Opt ReplicaID
  , replicaGroupID      :: Field f 'Req GroupID
  , replicaName         :: Field f 'Req T.Text
  , replicaDescription  :: Field f 'Opt (Maybe T.Text)
  , replicaWalletID     :: Field f 'Opt (Maybe WalletID)
  , replicaCreationTime :: Field f 'Opt UTCTime
  } deriving Generic

type Replica = Replica' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'Opt ReplicaID) (Field g 'Opt ReplicaID)
                  , Default p (Field f 'Req GroupID) (Field g 'Req GroupID)
                  , Default p (Field f 'Req T.Text) (Field g 'Req T.Text)
                  , Default p (Field f 'Opt (Maybe T.Text))
                              (Field g 'Opt (Maybe T.Text))
                  , Default p (Field f 'Opt (Maybe WalletID))
                              (Field g 'Opt (Maybe WalletID))
                  , Default p (Field f 'Opt UTCTime) (Field g 'Opt UTCTime)
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

validateReplica :: (MonadError MCError m, MonadReplica m) => Replica' Maybe
                -> m ()
validateReplica replica = undefined

{-
 - Effect
 -}

class MonadReplica m where
  placeholder :: m ()
