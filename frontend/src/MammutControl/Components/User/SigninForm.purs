module MammutControl.Components.User.SigninForm
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Either.Nested
import Data.Foldable (foldM)
import Data.Functor.Coproduct.Nested
import Data.Maybe (Maybe(..), maybe)

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.API.Helpers as API
import MammutControl.API.UserAPI as API
import MammutControl.Components.Common.InputField as InputField
import MammutControl.Components.UI.MenuBar as MenuBar
import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes

type State =
  { error    :: forall p i. Maybe (HH.HTML p i)
  , email    :: String
  , password :: String
  }

initialState :: State
initialState =
  { error: Nothing
  , email: ""
  , password: ""
  }

data Query a
  = ChangeEmail String a
  | ChangePassword String a
  | FormSubmitted Event a

type ChildQuery = Coproduct3 MenuBar.Query InputField.Query InputField.Query
type ChildSlot = Either3 Unit Unit Unit

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

render :: forall m. MonadAff m => State
       -> H.ParentHTML Query ChildQuery ChildSlot m
render st =
  HH.div_
    [ HH.slot' CP.cp1 unit MenuBar.component { route: Signin } absurd
    , HH.div [HP.classes [MHH.container, MHH.section]]
        [MHH.centeredDiv [renderForm st]]
    ]

renderForm :: forall m. MonadAff m => State
           -> H.ParentHTML Query ChildQuery ChildSlot m
renderForm st =
  MHH.form
    [ HE.onSubmit (HE.input FormSubmitted) ]
    [ case st.error of
        Nothing -> HH.text ""
        Just errHTML -> MHH.errorCard [errHTML]

    , MHH.box
        [ HH.slot' CP.cp2 unit InputField.component
            (InputField.defaultInput
              { name      = "email"
              , title     = "Email"
              , value     = st.email
              , inputType = HP.InputEmail
              }) (HE.input ChangeEmail)


        , HH.slot' CP.cp3 unit InputField.component
            (InputField.defaultInput
              { name      = "password"
              , title     = "Password"
              , value     = st.password
              , inputType = HP.InputPassword
              }) (HE.input ChangePassword)

        , MHH.inputField12 [MHH.submitButton [] [HH.text "Sign in"]]
        ]
      ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
eval = case _ of
  ChangeEmail email next -> do
    H.modify_ (_ { email = email })
    pure next

  ChangePassword password next -> do
    H.modify_ (_ { password = password })
    pure next

  FormSubmitted event next -> do
    liftEffect $ preventDefault event
    H.modify_ (_ { error = Nothing })
    st <- H.get
    response <- H.liftAff $ API.signin st
    case response of
      Left err -> processError err
      Right _ -> liftEffect $ window >>= location >>= setHash "/"
    pure next

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
processError = case _ of
  API.APIError _ msg _ mCode _ -> do
    let msg' = maybe msg API.humanReadableError mCode
    H.modify_ (_ { error = Just (HH.text msg') })

  API.MultipleAPIErrors _ errors -> do
    let step acc = case _ of
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
