module MammutControl.Components.Home
  ( component
  , Query
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.Components.UI.MenuBar as MenuBar
import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes

newtype Query a = Query Void

type State = Unit

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.parentComponent
  { initialState: const unit
  , render
  , eval: eval
  , receiver: const Nothing
  }

render :: forall m. MonadAff m => State
       -> H.ParentHTML Query MenuBar.Query Unit m
render _ =
  HH.div_
    [ HH.slot unit MenuBar.component { route: Home } absurd
    , HH.div [HP.classes [MHH.container, MHH.section]]
        [HH.p_ [ HH.text "Hello, world!" ]]
    ]

eval :: forall m. Query ~> H.ParentDSL State Query MenuBar.Query Unit Void m
eval (Query x) = absurd x
