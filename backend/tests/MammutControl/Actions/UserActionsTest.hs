module MammutControl.Actions.UserActionsTest
  ( userActionsTests
  ) where

import Control.Monad

import MammutControl.Actions.UserActions
import MammutControl.Data.User
import MammutControl.Data.UserGen

import TestHelpers

userActionsTests :: TestTree
userActionsTests = testGroup "MammutControl.Actions.UserActions"
  [ testProperty "The endpoints work together" $ withTests 10 $ property $ do
      email  <- forAll emailGen
      email' <- forAll emailGen
      name   <- forAll personNameGen
      name'  <- forAll personNameGen
      pwd    <- forAll passwordGen
      pwd'   <- forAll passwordGen
      guard $ email /= email' && name /= name' && pwd /= pwd'

      (eRes, (fdb, _)) <- runFakeDataT emptyFakeDB initTime $ do
        LoginInfo user _ <- runAction Nothing $
          createUserAction dummyJWTSettings $ UserCreationData email name pwd
        annotate "Account created."
        let uid = userID user
        _ <- runAction (Just uid) $ editUserAction uid $
          UserEditionData (Just pwd') $ emptyUser
            { userEmail = Just email'
            , userName  = Just name'
            }
        annotate "Account edited."
        LoginInfo user' _ <- runAction Nothing $
          signinAction dummyJWTSettings $ UserCredentials email' pwd'
        annotate "Signed in."
        _ <- runAction (Just uid) $ deleteUserAction uid
        annotate "Account deleted."
        return (user, user')

      case eRes of
        Left errs -> annotateShow errs >> failure
        Right (user, user') -> do
          userEmail user  === email
          userName  user  === name
          userEmail user' === email'
          userName  user' === name'
          assert $ null $ fdbUsers fdb
  ]
