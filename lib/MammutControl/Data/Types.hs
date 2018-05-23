{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module MammutControl.Data.Types
  ( module MammutControl.Data.Types
  , Connection
  , Pool
  , withResource
  , Int64
  , Generic
  , Identity
  ) where

import           GHC.Generics

import           Control.Arrow
import           Control.Exception (mask, onException)
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Pool (Pool, withResource)
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import           Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T

import           Database.PostgreSQL.Simple ( Connection, begin, commit
                                            , rollback )

import           Opaleye

import           MammutControl.Error

data TblCol a
data Col a
data WriteCol a
data Write a

type family ColumnType a
type instance ColumnType T.Text    = PGText
type instance ColumnType Int       = PGInt4
type instance ColumnType UTCTime   = PGTimestamptz
type instance ColumnType (Maybe a) = Nullable (ColumnType a)

data ReqOpt = Req | Opt

type family Field f req a where
  Field Identity _ a    = a
  Field TblCol r a      = TableColumns (Field WriteCol r a) (Field Col r a)
  Field Col _ a         = Column (ColumnType a)
  Field WriteCol Req a  = Column (ColumnType a)
  Field WriteCol Opt a  = Maybe (Column (ColumnType a))
  Field Write Req a     = a
  Field Write Opt a     = Maybe a
  Field f _ a           = f a

class HoistField a b where
  hoistField :: a -> b

instance {-# OVERLAPPABLE #-} a ~ b => HoistField a b where
  hoistField = id

instance HoistField a (Maybe a) where
  hoistField = Just

instance HoistField a b => HoistField (Maybe a) (Maybe b) where
  hoistField = fmap hoistField

instance Default Constant a (Column b) => HoistField a (Column b) where
  hoistField = constant

hoistFields :: forall t (f :: k) (g :: k).
               ( Generic (t f), Generic (t g)
               , GHoistFields (Rep (t f)) (Rep (t g)))
            => t f -> t g
hoistFields = to . ghoistFields . from

class GHoistFields f g where
  ghoistFields :: f i -> g i

instance GHoistFields U1 U1 where
  ghoistFields U1 = U1

instance GHoistFields a b => GHoistFields (M1 i c a) (M1 i' c' b) where
  ghoistFields = M1 . ghoistFields . unM1

instance HoistField a b => GHoistFields (K1 i a) (K1 i b) where
  ghoistFields = K1 . hoistField . unK1

instance (GHoistFields a c, GHoistFields b d)
    => GHoistFields (a :*: b) (c :*: d) where
  ghoistFields (x :*: y) = ghoistFields x :*: ghoistFields y

instance (GHoistFields a c, GHoistFields b d)
    => GHoistFields (a :+: b) (c :+: d) where
  ghoistFields (L1 x) = L1 $ ghoistFields x
  ghoistFields (R1 x) = R1 $ ghoistFields x

newtype DataM a
  = DataM { unDataM :: ReaderT (Either Connection (Pool Connection))
                               (ExceptT MCError IO) a }
  deriving newtype ( Applicative, Functor, Monad, MonadBase IO
                   , MonadError MCError
                   , MonadReader (Either Connection (Pool Connection))
                   )

runDataM :: DataM a -> Pool Connection -> IO (Either MCError a)
runDataM action pool =
  runExceptT $ flip runReaderT (Right pool) $ unDataM action

withConn :: (Connection -> IO a) -> DataM a
withConn f = do
  eConn <- ask
  case eConn of
    Left conn -> liftBase $ f conn
    Right pool -> liftBase $ withResource pool f

class Monad m => MonadTransaction m where
  withTransaction :: m a -> m a

instance MonadTransaction DataM where
  withTransaction action = do
    eConn <- ask
    case eConn of
      Left _ -> action
      Right _ ->  do
        eRes <- withConn $ \conn -> mask $ \restore -> do
          begin conn
          r <- restore (runExceptT (runReaderT (unDataM action) (Left conn)))
                 `onException` rollback conn
          case r of
            Left  _ -> rollback conn
            Right _ -> commit conn
          return r
        either throwError return eRes

class Monad m => MonadTime m where
  getTime :: m UTCTime

instance MonadTime DataM where
  getTime = liftBase getCurrentTime

instance MonadTime m => MonadTime (ReaderT r m) where
  getTime = lift getTime
