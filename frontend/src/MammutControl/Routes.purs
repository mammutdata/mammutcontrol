module MammutControl.Routes
  ( Route(..)
  , routes
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

data Route
  = Home
  | Signup
  | Signin

-- FIXME: How to derive this?
instance showRoute :: Show Route where
  show = case _ of
    Home   -> "Home"
    Signup -> "Signup"
    Signin -> "Signin"

routes :: Match Route
routes = root *> oneOf
  [ Signup <$ lit "signup"
  , Signin <$ lit "signin"
  , pure Home
  ] <* end
