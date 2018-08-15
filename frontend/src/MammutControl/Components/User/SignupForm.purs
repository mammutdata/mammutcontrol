module MammutControl.Components.User.SignupForm
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), fromMaybe, maybe)

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

import Web.Event.Event (Event, preventDefault)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.API.Helpers as API
import MammutControl.API.UserAPI as API
import MammutControl.Components.UI.MenuBar as MenuBar
import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes

type State =
  { error                :: forall p i. Maybe (HH.HTML p i)
  , name                 :: String
  , nameError            :: Maybe String
  , email                :: String
  , emailError           :: Maybe String
  , password             :: String
  , passwordConfirmation :: String
  , passwordError        :: Maybe String
  }

initialState :: State
initialState =
  { error: Nothing
  , name: ""
  , nameError: Nothing
  , email: ""
  , emailError: Nothing
  , password: ""
  , passwordConfirmation: ""
  , passwordError: Nothing
  }

data Query a
  = ChangeName String a
  | ChangeEmail String a
  | ChangePassword String a
  | ChangePasswordConfirmation String a
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
    [ HH.slot unit MenuBar.component { route: Signup } absurd
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

    , MHH.box
        [ MHH.inputWrapper
            [ MHH.input st.nameError
                [ HP.type_ HP.InputText
                , HP.value st.name
                , HP.name "name"
                , HP.autofocus true
                , HE.onValueInput (HE.input ChangeName)
                ]
            , HH.label [ HP.for "name" ] [ HH.text "Full name" ]
            , MHH.inputHelper "" st.nameError
            ]

        , MHH.inputWrapper
            [ MHH.input st.emailError
                [ HP.type_ HP.InputEmail
                , HP.value st.email
                , HP.name "email"
                , HE.onValueInput (HE.input ChangeEmail)
                ]
            , HH.label [ HP.for "email" ] [ HH.text "Email" ]
            , MHH.inputHelper "" st.emailError
            ]

        , MHH.row_
            [ MHH.inputField6
               [ MHH.inputNoValidate st.password st.passwordError
                   [ HP.type_ HP.InputPassword
                   , HP.value st.password
                   , HP.name "password"
                   , HE.onValueInput (HE.input ChangePassword)
                   ]
               , HH.label [ HP.for "password" ] [ HH.text "Password" ]
               , MHH.inputHelper "" st.passwordError
               ]

            , MHH.inputField6
                [ MHH.inputNoValidate st.passwordConfirmation Nothing
                    [ HP.type_ HP.InputPassword
                    , HP.value st.passwordConfirmation
                    , HP.name "password_confirmation"
                    , HE.onValueInput (HE.input ChangePasswordConfirmation)
                    ]
                , HH.label
                    [ HP.for "password_confirmation" ]
                    [ HH.text "Password confirmation" ]
                ]
            ]

        , MHH.submitButton []
            [ HH.text "Sign up" ]
        ]
    ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query MenuBar.Query Unit Void m
eval = case _ of
  ChangeName name next -> do
    H.modify_ (_ { name = name, nameError = Nothing })
    pure next

  ChangeEmail email next -> do
    H.modify_ (_ { email = email, emailError = Nothing })
    pure next

  ChangePassword password next -> do
    H.modify_ (_ { password = password, passwordError = Nothing })
    checkPasswords
    pure next

  ChangePasswordConfirmation password next -> do
    H.modify_ (_ { passwordConfirmation = password, passwordError = Nothing })
    checkPasswords
    pure next

  FormSubmitted event next -> do
    liftEffect $ preventDefault event
    st <- H.get
    when (st.password == st.passwordConfirmation) $ do
      H.modify_ (_ { error = Nothing })
      response <- H.liftAff $
        API.signup { name: st.name, email: st.email, password: st.password }
      case response of
        Left err -> processError err
        Right _ -> pure unit
    pure next

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query MenuBar.Query Unit Void m Unit
processError = case _ of
  API.APIError _ msg mLoc mCode _ -> do
    let msg' = maybe msg API.humanReadableError mCode
    case mLoc of
      Just "name"     -> H.modify_ (_ { nameError     = Just msg' })
      Just "email"    -> H.modify_ (_ { emailError    = Just msg' })
      Just "password" -> H.modify_ (_ { passwordError = Just msg' })
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

checkPasswords
  :: forall m. H.ParentDSL State Query MenuBar.Query Unit Void m Unit
checkPasswords = do
  st <- H.get
  when (st.password /= st.passwordConfirmation) $
    H.modify_ (_ { passwordError = Just "Passwords don't match." })
