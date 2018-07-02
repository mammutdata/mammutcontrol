import Test.Tasty

import MammutControl.Data.UserTest
import MammutControl.ErrorTest

main :: IO ()
main = defaultMain $ testGroup "Root"
  [ errorTests, userTests
  ]
