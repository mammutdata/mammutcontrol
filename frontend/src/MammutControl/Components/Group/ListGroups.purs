module MammutControl.Components.Group.ListGroups
  ( component
  , Query
  ) where

import Prelude

import Data.Array (cons)
import Data.Either (Either(..))
import Data.List
import Data.Maybe (Maybe(..))

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.API.GroupAPI as API
import MammutControl.API.Helpers as API
import MammutControl.Components.UI.MenuBar as MenuBar
import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes

type State =
  { groups :: Maybe (List API.Group)
  }

initialState :: State
initialState =
  { groups: Nothing
  }

data Query a
  = FetchGroups a

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (FetchGroups unit)
  , finalizer: Nothing
  }

render :: forall m. MonadAff m => State
       -> H.ParentHTML Query MenuBar.Query Unit m
render st =
  HH.div_
    [ HH.slot unit MenuBar.component { route: Groups } absurd
    , HH.div [HP.classes [MHH.container, MHH.section]]
        [ MHH.sectionTitle
            [ HH.text "Groups"
            , HH.div [HP.class_ (HH.ClassName "header__buttons")]
                     [MHH.addButton [HP.href "#/groups/new"]]
            ]
        , case st.groups of
            Nothing -> MHH.spinner
            Just groups -> renderGroups groups
        ]
    ]

renderGroups :: forall m. MonadAff m => List API.Group
             -> H.ParentHTML Query MenuBar.Query Unit m
renderGroups Nil =
  HH.p_ [HH.text "You are not a member of any group yet."]
renderGroups groups =
  HH.div_
    [ HH.p_ [HH.text "This list shows you all the groups you are a member of."]
    , HH.div_ [ HH.text "FIXME" ]
    ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query MenuBar.Query Unit Void m
eval = case _ of
  FetchGroups next -> do
    response <- H.liftAff API.getGroups
    case response of
      Left err -> processError err
      Right groups -> H.modify_ (_ { groups = Just groups })
    pure next

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query MenuBar.Query Unit Void m Unit
processError = case _ of
  _ -> pure unit
