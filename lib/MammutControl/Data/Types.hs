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

import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Pool (Pool, withResource)
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import qualified Data.Text as T

import           Database.PostgreSQL.Simple (Connection)

import           Opaleye

data TblCol a
data Col a
data WriteCol a
data Write a

type family ColumnType a
type instance ColumnType T.Text = PGText

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

instance HoistField a a where
  hoistField = id

instance HoistField a (Maybe a) where
  hoistField = Just

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
