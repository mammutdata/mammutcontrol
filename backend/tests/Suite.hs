{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Options

import MammutControl.Actions.GroupActionsTest
import MammutControl.Actions.UserActionsTest
import MammutControl.Data.GroupTest
import MammutControl.Data.UserTest
import MammutControl.Data.WalletTest
import MammutControl.ErrorTest

main :: IO ()
main = defaultMain allTests

ghcidMain :: IO ()
ghcidMain = do
  let transform = \case
        TestReporter ods g ->
          TestReporter ods $ \opts -> g (setOption (HideSuccesses True) opts)
        TestManager ods g ->
          TestManager ods $ \opts -> g (setOption (HideSuccesses True) opts)
      ingredients = map transform defaultIngredients

  defaultMainWithIngredients ingredients allTests

allTests :: TestTree
allTests = testGroup "Root"
  [ errorTests
  , groupActionsTests
  , groupTests
  , userActionsTests
  , userTests
  , walletTests
  ]
