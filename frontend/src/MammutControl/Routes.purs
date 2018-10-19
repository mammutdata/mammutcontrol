module MammutControl.Routes
  ( Route(..)
  , Section(..)
  , sectionFromRoute
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

import MammutControl.API.GroupAPI (GroupID(..))

data Route
  = Home
  | Signup
  | Signin
  | Groups
  | GroupsNew
  | Group GroupID

data Section
  = GroupSection

sectionFromRoute :: Route -> Maybe Section
sectionFromRoute = case _ of
  Home -> Nothing
  Signup -> Nothing
  Signin -> Nothing
  Groups -> Just GroupSection
  GroupsNew -> Just GroupSection
  Group _ -> Just GroupSection

routes :: Match Route
routes = root *> oneOf
  [ Signup <$ lit "signup"
  , Signin <$ lit "signin"
  -- Groups
  , GroupsNew <$ lit "groups" <* lit "new"
  , (Group <<< GroupID) <$ lit "groups" <*> str
  , Groups <$ lit "groups"
  -- /
  , pure Home
  ] <* end
