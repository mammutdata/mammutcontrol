module MammutControl.ErrorTest
  ( errorTests
  ) where

import Data.List.NonEmpty

import Servant

import MammutControl.Error

import TestHelpers

errorTests :: TestTree
errorTests = testGroup "MammutControl.Error" [toServantErrTests]

toServantErrTests :: TestTree
toServantErrTests = testGroup "toServantErr"
    [ testProperty "returns the same code if all the errors have the same\
                   \ one" $ once $ property $
        errHTTPCode (toServantErr (authErr :| [invalidTokenErr])) === 401

    , testProperty "returns the code 400 if all the errors have an error\
                   \ 4xx" $ once $ property $
        errHTTPCode (toServantErr (authErr :| [notFoundErr])) === 400

    , testProperty "returns the code 500 if there is at least one 5xx\
                   \ error" $ once $ property $
        errHTTPCode (toServantErr (authErr :| [internalErr])) === 500
    ]

  where
    authErr, invalidTokenErr, notFoundErr, internalErr :: MCError
    authErr = AuthenticationError "wrong password"
    invalidTokenErr = AccessDenied False "invalid token"
    notFoundErr = ResourceNotFoundError RTUser "user with ID 1"
    internalErr = InternalError "internal error"
