module MammutControl.Components.Router
  ( RouteChange(..)
  , component
  ) where

import Prelude

import Control.Applicative (pure, (<$), (<*), (*>))

import Data.Either.Nested
import Data.Foldable (oneOf)
import Data.Functor.Coproduct.Nested
import Data.Maybe

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Routing.Match

import MammutControl.Components.User.SigninForm as SigninForm
import MammutControl.Components.User.SignupForm as SignupForm
import MammutControl.Routes

data RouteChange a = RouteChange Route a

type ChildQuery = Coproduct2 SigninForm.Query SignupForm.Query

type ChildSlot = Either2 Unit Unit

component :: forall m. MonadAff m => H.Component HH.HTML RouteChange Unit Void m
component = H.parentComponent
  { initialState: const Home
  , render
  , eval
  , receiver: const Nothing
  }

render :: forall m. MonadAff m => Route
       -> H.ParentHTML RouteChange ChildQuery ChildSlot m
render route =
  let
    txt = case route of
            Home   -> HH.p_ [ HH.text "home" ]
            Signin -> HH.slot' CP.cp1 unit SigninForm.component unit absurd
            Signup -> HH.slot' CP.cp2 unit SignupForm.component unit absurd
  in
    HH.div_
      [ txt
      , HH.a [ HP.href "#/" ] [ HH.text "Home" ]
      , HH.a [ HP.href "#/signin" ] [ HH.text "Sign in" ]
      , HH.a [ HP.href "#/signup" ] [ HH.text "Sign up" ]
      ]

eval :: forall m. RouteChange
     ~> H.ParentDSL Route RouteChange ChildQuery ChildSlot Void m
eval (RouteChange route next) = do
  H.put route
  pure next
