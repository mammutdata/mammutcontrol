module MammutControl.Components.Group.NewGroup
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Int (fromString)
import Data.List
import Data.Maybe (Maybe(..), maybe)

import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Web.Event.Event (Event, preventDefault)

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
  { error            :: forall p i. Maybe (HH.HTML p i)
  , name             :: String
  , nameError        :: Maybe String
  , description      :: String
  , descriptionError :: Maybe String
  , walletID         :: Maybe Int
  }

initialState :: State
initialState =
  { error: Nothing
  , name: ""
  , nameError: Nothing
  , description: ""
  , descriptionError: Nothing
  , walletID: Nothing
  }

data Query a
  = ChangeName String a
  | ChangeDescription String a
  | ChangeWalletID Event a
  | FormSubmitted Event a
  | Init a

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (Init unit)
  , finalizer: Nothing
  }

render :: forall m. MonadAff m => State
       -> H.ParentHTML Query MenuBar.Query Unit m
render st =
  HH.div_
    [ HH.slot unit MenuBar.component { route: GroupsNew } absurd
    , HH.div [HP.classes [MHH.container, MHH.section]]
        [ MHH.sectionTitle [HH.text "Add a new group"]
        , HH.p_ [HH.text "Using this form, you can create a new group.\
                         \ Once created, you will be able to add or remove\
                         \ people from the group. You will also be able to add\
                         \ new replicas or move existing ones to this group."]
        ]
    , MHH.lightBand
        [ HH.div
            [HP.classes [MHH.container, MHH.section]]
            [MHH.centeredDiv [renderForm st]]
        ]
    ]

renderForm :: forall m. MonadAff m => State
           -> H.ParentHTML Query MenuBar.Query Unit m
renderForm st =
  MHH.form [HE.onSubmit (HE.input FormSubmitted)]
    [ case st.error of
        Nothing -> HH.text ""
        Just errHTML -> MHH.errorCard [errHTML]

    , MHH.inputWrapper
        [ MHH.input st.nameError
            [ HP.type_ HP.InputText
            , HP.value st.name
            , HP.name "name"
            , HP.autofocus true
            , HE.onValueInput (HE.input ChangeName)
            ]
        , HH.label [ HP.for "name" ] [ HH.text "Name" ]
        , MHH.inputHelper "" st.nameError
        ]

    , MHH.inputWrapper
        [ MHH.input st.descriptionError
            [ HP.type_ HP.InputText
            , HP.value st.description
            , HP.name "description"
            , HE.onValueInput (HE.input ChangeDescription)
            ]
        , HH.label [ HP.for "description" ] [ HH.text "Description" ]
        , MHH.inputHelper "" st.descriptionError
        ]

    , MHH.inputWrapper
        [ HH.label [ HP.for "wallet_id" ] [ HH.text "Wallet" ]
        , HH.div_
            [ HH.select
                [ HP.name "wallet_id"
                , HE.onChange (HE.input ChangeWalletID)
                ]
                [ HH.option [HP.selected true] [HH.text "No wallet"]
                , HH.option [HP.value "4365476"] [HH.text "Wallet 4365476"]
                ]
            ]
        , MHH.inputHelper "Optional default wallet to be charged when a backup\
                          \ is created in this group." Nothing
        ]

    , MHH.submitButton []
        [ HH.text "Add" ]
    ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query MenuBar.Query Unit Void m
eval = case _ of
  ChangeName name next -> do
    H.modify_ (_ { name = name, nameError = Nothing })
    pure next

  ChangeDescription description next -> do
    H.modify_ (_ { description = description, descriptionError = Nothing })
    pure next

  ChangeWalletID str next -> do
    liftEffect $ log "Hello"
--    let mwid = fromString $ str
--    H.modify_ (_ { walletID = mwid })
    pure next

  FormSubmitted event next -> do
    liftEffect $ preventDefault event
    pure next

  Init next -> do
    liftEffect MHH.initFormSelects
    pure next

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query MenuBar.Query Unit Void m Unit
processError = case _ of
  API.APIError _ msg mLoc mCode _ -> do
    let msg' = maybe msg API.humanReadableError mCode
    case mLoc of
      Just "name"        -> H.modify_ (_ { nameError        = Just msg' })
      Just "description" -> H.modify_ (_ { descriptionError = Just msg' })
      _ -> H.modify_ (_ { error = Just (HH.text msg') })

  API.MultipleAPIErrors _ errors -> do
    let step acc = case _ of
          err@(API.APIError _ _ (Just _) _ _) -> do
            processError err
            pure acc
          API.APIError _ msg _ mCode _ -> do
            let msg' = maybe msg API.humanReadableError mCode
            pure $ msg' : acc
          API.MultipleAPIErrors _ _ -> do
            liftEffect $
              log "the impossible happened: multiple errors in multiple errors"
            pure acc
          API.OtherError msg -> pure $ msg : acc

    msgs <- foldM step [] errors
    case msgs of
      [] -> pure unit
      _ -> do
        let msgList = API.humanReadableErrorList msgs
        H.modify_ (_ { error = Just msgList })

  API.OtherError msg -> H.modify_ (_ { error = Just (HH.text msg) })
