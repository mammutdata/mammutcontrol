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
import Halogen.Component.ChildPath
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Routing.Match

import MammutControl.API.GroupAPI (GroupID)
import MammutControl.Components.Group.ListGroups as ListGroups
import MammutControl.Components.Group.NewGroup as NewGroup
import MammutControl.Components.Group.ShowGroup as ShowGroup
import MammutControl.Components.User.SigninForm as SigninForm
import MammutControl.Components.User.SignupForm as SignupForm
import MammutControl.Components.Home as Home
import MammutControl.Routes

data RouteChange a = RouteChange Route a

type GeneralChildQuery = Coproduct3 Home.Query SigninForm.Query SignupForm.Query
type GroupChildQuery = Coproduct3 ListGroups.Query NewGroup.Query ShowGroup.Query
type ChildQuery = Coproduct2 GeneralChildQuery GroupChildQuery

type GeneralChildSlot = Either3 Unit Unit Unit
type GroupChildSlot = Either3 Unit Unit Unit
type ChildSlot = Either2 GeneralChildSlot GroupChildSlot

component :: forall m. MonadAff m => H.Component HH.HTML RouteChange Unit Void m
component = H.parentComponent
  { initialState: const Home
  , render
  , eval
  , receiver: const Nothing
  }

render :: forall m. MonadAff m => Route
       -> H.ParentHTML RouteChange ChildQuery ChildSlot m
render = case _ of
  Home      -> HH.slot' (cp1 :> cp1) unit Home.component       unit absurd
  Signin    -> HH.slot' (cp1 :> cp2) unit SigninForm.component unit absurd
  Signup    -> HH.slot' (cp1 :> cp3) unit SignupForm.component unit absurd
  Groups    -> HH.slot' (cp2 :> cp1) unit ListGroups.component unit absurd
  GroupsNew -> HH.slot' (cp2 :> cp2) unit NewGroup.component   unit absurd
  Group gid -> HH.slot' (cp2 :> cp3) unit ShowGroup.component  gid  absurd

eval :: forall m. RouteChange
     ~> H.ParentDSL Route RouteChange ChildQuery ChildSlot Void m
eval (RouteChange route next) = do
  H.put route
  pure next
