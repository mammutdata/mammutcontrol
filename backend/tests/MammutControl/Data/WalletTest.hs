module MammutControl.Data.WalletTest
  ( walletTests
  ) where

import Hedgehog.Gen as Gen

import MammutControl.Data.Wallet
import MammutControl.Data.User
import MammutControl.Data.UserGen
import TestHelpers

walletTests :: TestTree
walletTests = testGroup "MammutControl.Data.Wallet"
  [ createWalletTests
  ]

createWalletTests :: TestTree
createWalletTests = testGroup "createWallet"
  [ testProperty "creates a wallet with a given first member" $ property $ do
      name  <- forAll personNameGen
      email <- forAll emailGen
      pwd   <- forAll passwordGen

      wname <- forAll nameGen
      descr <- forAll $ Gen.choice
        [Gen.constant Nothing, Just <$> descriptionGen]

      eRes <- runStubbedDataT_ $ do
        user <- ignoreAccessControl $ createUserFromData email name pwd
        let wallet = Wallet
              { walletID           = ()
              , walletName         = wname
              , walletDescription  = descr
              , walletCredits      = ()
              , walletCreationTime = ()
              }
        wallet' <- createWallet wallet $ userID user
        users   <- getUsersByWalletID $ walletID wallet'
        return (wallet', users)

      case eRes of
        Left errs -> annotateShow errs >> failure
        Right (wallet, users) -> do
          walletName wallet         === wname
          walletDescription wallet  === descr
          walletCreationTime wallet === initTime
          case users of
            [user] -> do
              userEmail user === email
              userName user  === name
            _ -> annotateShow (length users) >> failure
  ]
