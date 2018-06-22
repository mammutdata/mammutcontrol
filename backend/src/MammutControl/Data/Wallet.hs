{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module MammutControl.Data.Wallet
  ( WalletID(..)
  , Wallet'(..)
  , Wallet
  , MonadWallet(..)
  , createWallet
  ) where

import           Control.Arrow
import           Control.Monad.MultiExcept (MonadMultiError, throwError)

import           Data.Aeson
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Adaptor
import           Data.Profunctor.Product.Default
import           Data.Time
import qualified Data.Text as T

import           Opaleye

import           Servant (FromHttpApiData(..))

import           MammutControl.Data.Types
import           MammutControl.Data.User
import           MammutControl.Error

{-
 - Database
 -}

newtype WalletID = WalletID { unWalletID :: Int64 } deriving Eq

type instance ColumnType WalletID = PGInt8

deriving newtype instance QueryRunnerColumnDefault PGInt8 WalletID
deriving newtype instance FromHttpApiData WalletID
deriving newtype instance FromJSON WalletID
deriving newtype instance ToJSON WalletID

instance Default Constant WalletID (Column PGInt8) where
  def = lmap unWalletID def

data Wallet' f = Wallet
  { walletID           :: Field f 'Opt WalletID
  , walletName         :: Field f 'Req T.Text
  , walletDescription  :: Field f 'Opt (Maybe T.Text)
  , walletCredits      :: Field f 'Opt Int
  , walletCreationTime :: Field f 'Opt UTCTime
  } deriving Generic

type Wallet = Wallet' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'Opt WalletID) (Field g 'Opt WalletID)
                  , Default p (Field f 'Req T.Text) (Field g 'Req T.Text)
                  , Default p (Field f 'Opt (Maybe T.Text))
                              (Field g 'Opt (Maybe T.Text))
                  , Default p (Field f 'Opt Int) (Field g 'Opt Int)
                  , Default p (Field f 'Opt UTCTime) (Field g 'Opt UTCTime)
                  ) => Default p (Wallet' f) (Wallet' g)

instance ToJSON Wallet where
  toJSON Wallet{..} = object $
    [ "id"      .= walletID
    , "name"    .= walletName
    , "credits" .= walletCredits
    ] ++ maybe [] (pure . ("description" .=)) walletDescription

pWallet :: Wallet' TblCol -> TableColumns (Wallet' WriteCol) (Wallet' Col)
pWallet = genericAdaptor

walletTable :: Table (Wallet' WriteCol) (Wallet' Col)
walletTable = table "active_wallets" $ pWallet Wallet
  { walletID           = tableColumn "id"
  , walletName         = tableColumn "name"
  , walletDescription  = tableColumn "description"
  , walletCredits      = tableColumn "credits"
  , walletCreationTime = tableColumn "creation_time"
  }

data WalletMembership' f = WalletMembership
  { wmUserID   :: Field f 'Req UserID
  , wmWalletID :: Field f 'Req WalletID
  } deriving Generic

type WalletMembership = WalletMembership' Identity

deriving instance ( ProductProfunctor p
                  , Default p (Field f 'Req UserID)   (Field g 'Req UserID)
                  , Default p (Field f 'Req WalletID) (Field g 'Req WalletID)
                  ) => Default p (WalletMembership' f) (WalletMembership' g)

pWalletMembership :: WalletMembership' TblCol
                  -> TableColumns (WalletMembership' WriteCol)
                                  (WalletMembership' Col)
pWalletMembership = genericAdaptor

walletMembershipTable :: Table (WalletMembership' WriteCol)
                               (WalletMembership' Col)
walletMembershipTable =
  table "wallet_memberships" $ pWalletMembership WalletMembership
    { wmUserID   = tableColumn "user_id"
    , wmWalletID = tableColumn "wallet_id"
    }

wmByUserID :: QueryArr (Column (ColumnType UserID)) (WalletMembership' Col)
wmByUserID = proc uid -> do
  wm <- queryTable walletMembershipTable -< ()
  restrict -< wmUserID wm .== uid
  returnA -< wm

wmByWalletID :: QueryArr (Column (ColumnType WalletID)) (WalletMembership' Col)
wmByWalletID = proc wid -> do
  wm <- queryTable walletMembershipTable -< ()
  restrict -< wmWalletID wm .== wid
  returnA -< wm

--walletByID :: QueryArr (Column (ColumnType WalletID)) (Wallet' Col)
--walletByID = proc wid -> do
--  wallet <- queryTable walletTable -< ()
--  restrict -< walletID wallet .== wid
--  returnA -< wallet

walletsByUserID :: QueryArr (Column (ColumnType UserID)) (Wallet' Col)
walletsByUserID = proc uid -> do
  wallet <- queryTable walletTable -< ()
  wm <- wmByUserID -< uid
  restrict -< walletID wallet .== wmWalletID wm
  returnA -< wallet

usersByWalletID :: QueryArr (Column (ColumnType WalletID)) (User' Col)
usersByWalletID = proc wid -> do
  wm <- wmByWalletID -< wid
  userByID -< wmUserID wm

{-
 - Logic
 -}

createWallet :: (MonadMultiError MCError m, MonadTransaction m, MonadWallet m)
             => Wallet' Write -> UserID -> m Wallet
createWallet wallet uid = withTransaction $ do
  wallet' <- createWalletNoOwner wallet
  addUserToWallet (walletID wallet') uid
  return wallet'

{-
 - Effect
 -}

class MonadWallet m where
  createWalletNoOwner :: Wallet' Write -> m Wallet
  addUserToWallet     :: WalletID -> UserID -> m ()
  getWalletsByUserID  :: UserID -> m [Wallet]
  getUsersByWalletID  :: WalletID -> m [User]

instance MonadWallet DataM where
  createWalletNoOwner = createWalletNoOwnerDataM
  addUserToWallet     = addUserToWalletDataM
  getWalletsByUserID  = getWalletsByUserIDDataM
  getUsersByWalletID  = getUsersByWalletIDDataM

createWalletNoOwnerDataM :: Wallet' Write -> DataM Wallet
createWalletNoOwnerDataM wallet = do
  res <- withConn $ \conn ->
    runInsertManyReturning conn walletTable [hoistFields wallet] id

  case res of
    wallet' : _ -> return wallet'
    [] -> throwError $ ValidationError Nothing "could not create wallet"

addUserToWalletDataM :: WalletID -> UserID -> DataM ()
addUserToWalletDataM wid uid = do
  let membership = hoistFields (WalletMembership uid wid :: WalletMembership)
  res <- withConn $ \conn ->
    runInsertMany conn walletMembershipTable [membership]

  if res > 0
    then return ()
    else throwError $ ValidationError Nothing "could not add user to wallet"

getWalletsByUserIDDataM :: UserID -> DataM [Wallet]
getWalletsByUserIDDataM uid = withConn $ \conn ->
  runQuery conn $ walletsByUserID <<^ \() -> constant uid

getUsersByWalletIDDataM :: WalletID -> DataM [User]
getUsersByWalletIDDataM wid = withConn $ \conn ->
  runQuery conn $ usersByWalletID <<^ \() -> constant wid
