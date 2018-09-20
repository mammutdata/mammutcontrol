module MammutControl.Actions.WalletActions
  ( getWalletsAction
  , WalletData
  , createWalletAction
  ) where

import           Data.Aeson
import qualified Data.Text as T

import           MammutControl.Actions.Helpers
import           MammutControl.Data.Wallet

getWalletsAction :: MonadAction m => Session
                 -> m (JSONWrapper "wallets" [Wallet])
getWalletsAction = fmap JSONWrapper . getWalletsByUserID . sessionUserID

data WalletData = WalletData T.Text (Maybe T.Text)

instance FromJSON WalletData where
  parseJSON = withObject "wallet data" $ \obj -> WalletData
    <$> obj .: "name"
    <*> obj .:? "description"

createWalletAction :: MonadAction m => Session -> WalletData -> m Wallet
createWalletAction session (WalletData name mDescription) = do
  let wallet = Wallet
        { walletID           = ()
        , walletName         = name
        , walletDescription  = mDescription
        , walletCredits      = ()
        , walletCreationTime = ()
        }
  createWallet wallet (sessionUserID session)
