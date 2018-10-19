module MammutControl.Actions.WalletActions
  ( getWalletAction
  , getWalletsAction
  , WalletData
  , createWalletAction
  ) where

import           Data.Aeson
import qualified Data.Text as T

import           MammutControl.Actions.Helpers
import           MammutControl.Data.Wallet

getWalletAction :: MonadAction m => WalletID -> m (JSONWrapper "wallet" Wallet)
getWalletAction = fmap JSONWrapper . getWallet

getWalletsAction :: MonadAction m => Session
                 -> m (JSONWrapper "wallets" [Wallet])
getWalletsAction = fmap JSONWrapper . getWalletsByUserID . sessionUserID

data WalletData = WalletData T.Text (Maybe T.Text)

instance FromJSON WalletData where
  parseJSON = withObject "wallet data" $ \obj -> WalletData
    <$> obj .: "name"
    <*> obj .:? "description"

createWalletAction :: MonadAction m => Session -> WalletData
                   -> m (JSONWrapper "wallet" Wallet)
createWalletAction session (WalletData name mDescription) = do
  let wallet = Wallet
        { walletID           = ()
        , walletName         = name
        , walletDescription  = mDescription
        , walletCredits      = ()
        , walletCreationTime = ()
        }
  JSONWrapper <$> createWallet wallet (sessionUserID session)
