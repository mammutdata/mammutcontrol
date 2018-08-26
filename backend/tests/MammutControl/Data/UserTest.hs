module MammutControl.Data.UserTest
  ( userTests
  ) where

import Control.Monad

import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty(..), toList)

import MammutControl.Data.User
import MammutControl.Data.UserGen
import MammutControl.Error

import TestHelpers

userTests :: TestTree
userTests = testGroup "MammutControl.Data.User"
  [ createUserFromDataTests, validatePasswordTests, validateUserTests
  , editUserTests
  ]

createUserFromDataTests :: TestTree
createUserFromDataTests = testGroup "createUserFromData"
  [ testProperty "creates a new user from basic data" $ once $ property $ do
      let hash = PasswordHash "hashed"

      eUser <- runStubbedDataT_ $ do
        stub $ HashPassword $ \pwd ->
          return $ if pwd == "secret"
            then StubResult hash
            else StubNotMatched
        ignoreAccessControl $ createUserFromData "bob@email.com" "Bob" "secret"

      case eUser of
        Left errs -> annotateShow errs >> failure
        Right user -> do
          userPasswordHash user === hash
          userCreationTime user === initTime
          userEmail user        === "bob@email.com"
          userName user         === "Bob"

  , testProperty "throws a validation error if the email is\
                 \ taken" $ once $ property $ do
      eRes <- runStubbedDataT_ $ ignoreAccessControl $ do
        _ <- createUserFromData "bob@email.com" "Bob" "secret"
        createUserFromData "bob@email.com" "Name" "secret"

      case eRes of
        Left (ValidationError (Just ("email", AlreadyTaken)) _ :| []) ->
          return ()
        Left errs -> annotateShow errs >> failure
        Right _ -> failure
  ]

validatePasswordTests :: TestTree
validatePasswordTests = testGroup "validatePassword"
  [ testProperty "returns () when the password is\
                 \ correct" $ withTests 10 $ property $ do
      pwd <- forAll passwordGen
      eRes <- runStubbedDataT_ $ ignoreAccessControl $  do
        user <- createUserFromData "bob@email.tld" "Bob" pwd
        validatePassword user pwd
      eRes === Right ()

  , testProperty "throws an authentication error when the\
                 \ password is wrong" $ withTests 10 $ property $ do
      pwd  <- forAll passwordGen
      pwd' <- forAll passwordGen
      guard $ pwd /= pwd'

      eRes <- runStubbedDataT_ $ ignoreAccessControl $ do
        user <- createUserFromData "bob@email.tld" "Bob" pwd
        validatePassword user pwd'

      case eRes of
        Left (AuthenticationError _ :| []) -> return ()
        _ -> annotateShow eRes >> failure

  , testProperty "rehashes the password if the policy has\
                 \ changed" $ withTests 10 $ property $ do
      pwd <- forAll passwordGen
      let unsecureHash =
            hashPasswordForTests (passwordHashingCostForTests - 1) pwd
      eRes <- runStubbedDataT_ $ do
        user <- createUser User
          { userID           = ()
          , userName         = "Alice"
          , userEmail        = "alice@email.tld"
          , userPasswordHash = unsecureHash
          , userCreationTime = ()
          }
        stub $ EditUser $ \_ fields -> do
          secureHash <- hashPassword pwd
          return $ if userPasswordHash fields == Just secureHash
            then StubMatched
            else StubNotMatched
        validatePassword user pwd
      eRes === Right ()
  ]

validateUserTests :: TestTree
validateUserTests = testGroup "validateUser"
  [ testProperty "returns multiple errors if there are several\
                 \ ones" $ once $ property $ do
      eRes <- runFakeDataT_ $ ignoreAccessControl $ do
        _ <- createUserFromData "bob@email.tld" "Bob" "secret"
        validateUser User
          { userID           = Nothing
          , userName         = Just ""
          , userEmail        = Just "bob@email.tld"
          , userPasswordHash = Nothing
          , userCreationTime = Nothing
          }
      case eRes of
        Left errs -> do
          let errs' = toList errs
          annotateShow errs'
          assert $ flip any errs' $ \case
            ValidationError (Just ("name", CantBeEmpty)) _ -> True
            _ -> False
          assert $ flip any errs' $ \case
            ValidationError (Just ("email", AlreadyTaken)) _ -> True
            _ -> False
        _ -> failure
  ]

editUserTests :: TestTree
editUserTests = testGroup "editUser"
  [ testProperty "validates the user before editing it" $ once $ property $ do
      (eRes, (fdb, _)) <-
        runFakeDataT emptyFakeDB initTime $ ignoreAccessControl $ do
          user <- createUserFromData "bob@email.tld" "Bob" "secret"
          editUser (userID user) $ emptyUser
            { userName = Just "", userEmail = Just "alice@email.tld" }

      when (isRight eRes) failure
      case fdbUsers fdb of
        [user] -> do
          annotateShow $ fmap (const ()) eRes -- we know it's Left
          userEmail user === "bob@email.tld"
        _ -> failure

  , testProperty "edits a user" $ once $ property $ do
      eUser <- runFakeDataT_ $ ignoreAccessControl $ do
        user <- createUserFromData "bob@email.tld" "Bob" "secret"
        _ <- editUser (userID user) $ emptyUser
          { userName         = Just "Alice"
          , userEmail        = Just "alice@email.tld"
          , userPasswordHash = Just (PasswordHash "other-secret-hash")
          }
        getUser $ userID user

      case eUser of
        Left errs -> annotateShow errs >> failure
        Right user -> do
          userName user         === "Alice"
          userEmail user        === "alice@email.tld"
          userPasswordHash user === PasswordHash "other-secret-hash"
  ]
