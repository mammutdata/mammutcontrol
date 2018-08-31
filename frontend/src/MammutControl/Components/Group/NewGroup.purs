module MammutControl.Components.Group.NewGroup
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Either.Nested
import Data.Foldable (foldM)
import Data.Functor.Coproduct.Nested
import Data.Int (fromString)
import Data.List
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Web.Event.Event (Event, preventDefault)

import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.API.GroupAPI as API
import MammutControl.API.Helpers as API
import MammutControl.Components.Common.InputField as InputField
import MammutControl.Components.Common.SelectField as SelectField
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
  | ChangeWalletID String a
  | FormSubmitted Event a
  | Init a

type ChildQuery
  = Coproduct4 MenuBar.Query InputField.Query InputField.Query SelectField.Query
type ChildSlot = Either4 Unit Unit Unit Unit

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
       -> H.ParentHTML Query ChildQuery ChildSlot m
render st =
  HH.div_
    [ HH.slot' CP.cp1 unit MenuBar.component { route: GroupsNew } absurd
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
           -> H.ParentHTML Query ChildQuery ChildSlot m
renderForm st =
  MHH.form [HE.onSubmit (HE.input FormSubmitted)]
    [ case st.error of
        Nothing -> HH.text ""
        Just errHTML -> MHH.errorCard [errHTML]

    , HH.slot' CP.cp2 unit InputField.component
        (InputField.defaultInput
          { name      = "name"
          , title     = "Name"
          , error     = st.nameError
          , value     = st.name
          , autofocus = true
          }) (HE.input ChangeName)

    , HH.slot' CP.cp3 unit InputField.component
        (InputField.defaultInput
          { name  = "description"
          , title = "Description"
          , error = st.descriptionError
          , value = st.description
          }) (HE.input ChangeDescription)

    , HH.slot' CP.cp4 unit SelectField.component
        (SelectField.defaultInput
          { value       = "FIXME"
          , name        = "wallet_id"
          , title       = "Wallet"
          , description = "Optional default wallet to be charged when a backup\
                          \ is created in this group."
          , options     = [Tuple "" "No wallet", Tuple "FIXME" "FIXME"]
          }) (HE.input ChangeWalletID)

    , MHH.inputField12 [MHH.submitButton [] [HH.text "Add"]]
    ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
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
             -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
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
