module MammutControl.Components.User.SignupForm
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Either.Nested
import Data.Foldable (foldM)
import Data.Functor.Coproduct.Nested
import Data.Maybe (Maybe(..), fromMaybe, maybe)

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

import Web.Event.Event (Event, preventDefault)

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

type ChildQuery
  = Coproduct5 MenuBar.Query InputField.Query InputField.Query InputField.Query
               InputField.Query
type ChildSlot = Either5 Unit Unit Unit Unit Unit

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
    [ HH.slot' CP.cp1 unit MenuBar.component { route: Signup } absurd
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
              { name      = "name"
              , title     = "Full name"
              , error     = st.nameError
              , value     = st.name
              , autofocus = true
              }) (HE.input ChangeName)

        , HH.slot' CP.cp3 unit InputField.component
            (InputField.defaultInput
              { name      = "email"
              , title     = "Email"
              , error     = st.emailError
              , value     = st.email
              , inputType = HP.InputEmail
              }) (HE.input ChangeEmail)

        , HH.slot' CP.cp4 unit InputField.component
            (InputField.defaultInput
              { name      = "password"
              , title     = "Password"
              , error     = st.passwordError
              , value     = st.password
              , inputType = HP.InputPassword
              , wrapper   = MHH.inputField6
              }) (HE.input ChangePassword)

        , HH.slot' CP.cp5 unit InputField.component
            (InputField.defaultInput
              { name      = "password_confirmation"
              , title     = "Password confirmation"
              , value     = st.passwordConfirmation
              , inputType = HP.InputPassword
              , wrapper   = MHH.inputField6
              }) (HE.input ChangePasswordConfirmation)

        , MHH.inputField12 [MHH.submitButton [] [HH.text "Sign up"]]
        ]
    ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
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
      response <- H.liftAff $ API.signup st
      case response of
        Left err -> processError err
        Right _ -> pure unit
    pure next

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
processError = case _ of
  API.APIError _ msg mLoc mCode _ -> do
    let msg' = maybe msg API.humanReadableErrorCode mCode
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
            let msg' = maybe msg API.humanReadableErrorCode mCode
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
        let msgList = API.errorsHTML msgs
        H.modify_ (_ { error = Just msgList })

  API.OtherError msg -> H.modify_ (_ { error = Just (HH.text msg) })

checkPasswords
  :: forall m. H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
checkPasswords = do
  st <- H.get
  when (st.password /= st.passwordConfirmation) $
    H.modify_ (_ { passwordError = Just "Passwords don't match." })
