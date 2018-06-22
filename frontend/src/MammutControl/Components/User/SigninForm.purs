module MammutControl.Components.User.SigninForm
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (foldM)
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
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.API as API
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

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

render :: forall m. MonadAff m => State
       -> H.ParentHTML Query MenuBar.Query Unit m
render st =
  HH.div_
    [ HH.slot unit MenuBar.component { route: Signin, loggedIn: false } absurd
    , HH.div [HP.classes [MHH.container, MHH.section]]
        [MHH.centeredDiv [renderForm st]]
    ]

renderForm :: forall m. MonadAff m => State
           -> H.ParentHTML Query MenuBar.Query Unit m
renderForm st =
  MHH.form
    [ HE.onSubmit (HE.input FormSubmitted) ]
    [ case st.error of
        Nothing -> HH.text ""
        Just errHTML -> MHH.errorCard [errHTML]

    , MHH.inputWrapper
        [ MHH.input Nothing
            [ HP.type_ HP.InputText
            , HP.value st.email
            , HP.name "email"
            , HP.autofocus true
            , HE.onValueInput (HE.input ChangeEmail)
            ]
        , HH.label [ HP.for "email" ] [ HH.text "Email" ]
        ]

    , MHH.inputWrapper
        [ MHH.input Nothing
            [ HP.type_ HP.InputPassword
            , HP.value st.password
            , HP.name "password"
            , HE.onValueInput (HE.input ChangePassword)
            ]
        , HH.label [ HP.for "password" ] [ HH.text "Password" ]
        ]

    , MHH.submitButton []
        [ HH.text "Sign in" ]
    ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query MenuBar.Query Unit Void m
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
    response <- H.liftAff $
      API.signin { email: st.email, password: st.password }
    case response of
      Left err -> processError err
      Right _ -> liftEffect $ window >>= location >>= setHash "/"
    pure next

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query MenuBar.Query Unit Void m Unit
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
