module MammutControl.Actions.GroupActionsTest
  ( groupActionsTests
  ) where

import Control.Monad

import Data.Maybe

import Hedgehog.Gen as Gen

import MammutControl.Actions.GroupActions
import MammutControl.Actions.Helpers hiding (runAction)
import MammutControl.Data.Group
import MammutControl.Data.Types
import MammutControl.Data.User
import MammutControl.Data.UserGen
import MammutControl.Data.Wallet

import TestHelpers

groupActionsTests :: TestTree
groupActionsTests = testGroup "MammutControl.Actions.GroupActions"
  [ createGroupActionTests
  , addUserToGroupActionTests
  , removeUserGromGroupActionTests
  ]

createGroupActionTests :: TestTree
createGroupActionTests = testGroup "createGroupAction"
  [ testProperty "creates a new group owned by the current\
                 \ user" $ property $ do
      name       <- forAll nameGen
      descr      <- forAll $ Gen.maybe descriptionGen
      withWallet <- forAll Gen.bool

      eRes <- runStubbedDataT_ $ runActionWithUser $ \user -> do
        mwid <- if withWallet
          then do
            let wallet = Wallet
                  { walletID           = ()
                  , walletName         = name
                  , walletDescription  = descr
                  , walletCredits      = ()
                  , walletCreationTime = ()
                  }
            wallet' <- createWallet wallet (userID user)
            return $ Just $ walletID wallet'
          else return Nothing

        group <- createGroupAction (Session (userID user)) $
          GroupData name descr mwid

        members <- getUsersByGroupID $ groupID group
        return (group, members)

      case eRes of
        Left errs -> annotateShow errs >> failure
        Right (group, members) -> do
          groupName        group === name
          groupDescription group === descr
          isJust (groupWalletID group) === withWallet
          length members === 1

  , testProperty "prevents the user from using a wallet she doesn't\
                 \ own" $ once $ property $ do
      gname  <- forAll nameGen
      gdescr <- forAll $ Gen.maybe descriptionGen
      wname  <- forAll nameGen
      wdescr <- forAll $ Gen.maybe descriptionGen

      eRes <- runStubbedDataT_ $ do
        bob <- ignoreAccessControl $
          createUserFromData "bob@email.tld" "Bob" "secret"
        let wallet' = Wallet
              { walletID           = ()
              , walletName         = wname
              , walletDescription  = wdescr
              , walletCredits      = ()
              , walletCreationTime = ()
              }
        wallet <- createWallet wallet' (userID bob)

        runActionWithUser $ \mallory -> do
          createGroupAction (Session (userID mallory)) $
            GroupData gname gdescr (Just (walletID wallet))

      assertAccessDenied eRes
  ]

addUserToGroupActionTests :: TestTree
addUserToGroupActionTests = testGroup "addUserToGroupAction"
  [ testProperty "adds a user by its email to a group" $ property $ do
      gname  <- forAll nameGen
      gdescr <- forAll $ Gen.maybe descriptionGen

      email <- forAll emailGen
      uname <- forAll personNameGen
      pwd   <- forAll passwordGen

      twice <- forAll Gen.bool

      eRes <- runStubbedDataT_ $ do
        _ <- ignoreAccessControl $ createUserFromData email uname pwd

        runActionWithUser $ \otherUser -> do
          group <- createGroupAction (Session (userID otherUser)) $
            GroupData gname gdescr Nothing
          when twice $ void $
            addUserToGroupAction (groupID group) $ GroupMembershipData email
          addUserToGroupAction (groupID group) $ GroupMembershipData email

      case eRes of
        Left  errs  -> annotateShow errs >> failure
        Right users -> length users === 2

  , testProperty "prevents the user from adding users to a group without\
                 \ membership" $ once $ property $ do
      eRes <- runStubbedDataT_ $ do
        bob <- ignoreAccessControl $
          createUserFromData "bob@email.tld" "Bob" "secret"
        _ <- ignoreAccessControl $
          createUserFromData "alice@email.tld" "Alice" "secret"
        mallory <- ignoreAccessControl $
          createUserFromData "mallory@email.tld" "Mallory" "secret"
        group <- createGroup myCompanyGroupWrite (userID bob)

        runAction (Just (userID mallory)) $ do
          addUserToGroupAction (groupID group) $
            GroupMembershipData "alice@email.tld"

      assertAccessDenied eRes
  ]

removeUserGromGroupActionTests :: TestTree
removeUserGromGroupActionTests = testGroup "removeUserFromGroup"
  [ testProperty "prevents a user from deleting user from group without\
                 \ membership" $ once $ property $ do
      eRes <- runFakeDataT_ $ do
        alice <- ignoreAccessControl $
          createUserFromData "Alice" "alice@email.tld" "secret"
        bob   <- ignoreAccessControl $
          createUserFromData "Bob"   "bob@email.tld" "secret"
        cecil <- ignoreAccessControl $
          createUserFromData "Cecil" "cecil@email.tld" "secret"
        group <- createGroup myCompanyGroupWrite (userID alice)
        addUserToGroup (groupID group) (userID bob)

        runAction (Just (userID cecil)) $ do
          removeUserFromGroupAction (groupID group) (userID bob)

      assertAccessDenied eRes
  ]

myCompanyGroupWrite :: Group' Write
myCompanyGroupWrite = Group
  { groupID           = ()
  , groupName         = "My company"
  , groupDescription  = Just "All sysadmins of my company"
  , groupWalletID     = Nothing
  , groupCreationTime = ()
  }
