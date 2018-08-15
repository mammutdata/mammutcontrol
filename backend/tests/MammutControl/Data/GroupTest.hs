module MammutControl.Data.GroupTest
  ( groupTests
  ) where

import Hedgehog.Gen as Gen

import MammutControl.Data.Group
import MammutControl.Data.User
import MammutControl.Data.UserGen

import TestHelpers

groupTests :: TestTree
groupTests = testGroup "MammutControl.Data.Group"
  [ createGroupTests
  ]

createGroupTests :: TestTree
createGroupTests = testGroup "createGroup"
  [ testProperty "creates a group with a given first member" $ property $ do
      name  <- forAll personNameGen
      email <- forAll emailGen
      pwd   <- forAll passwordGen

      gname <- forAll nameGen
      descr <- forAll $ Gen.maybe descriptionGen

      eRes <- runStubbedDataT_ $ do
        user <- createUserFromData email name pwd
        let group = Group
              { groupID           = ()
              , groupName         = gname
              , groupDescription  = descr
              , groupWalletID     = Nothing
              , groupCreationTime = ()
              }
        group' <- createGroup group $ userID user
        users  <- getUsersByGroupID $ groupID group'
        return (group', users)

      case eRes of
        Left errs -> annotateShow errs >> failure
        Right (group, users) -> do
          groupName group         === gname
          groupDescription group  === descr
          groupWalletID group     === Nothing
          groupCreationTime group === initTime
          case users of
            [user] -> do
              userEmail user === email
              userName user  === name
            _ -> annotateShow (length users) >> failure
  ]
