module MammutControl.Components.Common.InputField
  ( component
  , Input(..)
  , defaultInput
  , Query(..)
  , Message
  ) where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.HTMLHelpers as MHH

type Input =
  { inputType   :: HP.InputType
  , value       :: String
  , name        :: String
  , title       :: String
  , description :: String
  , error       :: Maybe String
  , autofocus   :: Boolean
  , wrapper     :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
  }

defaultInput :: Input
defaultInput =
  { inputType: HP.InputText
  , value: ""
  , name: ""
  , title: ""
  , description: ""
  , error: Nothing
  , autofocus: false
  , wrapper: MHH.inputWrapper
  }

type Message = String

type State = { input :: Input }

data Query a
  = ChangeInput Input a
  | ChangeValue String a

component :: forall m. H.Component HH.HTML Query Input Message m
component = H.component
  { initialState: \input -> { input }
  , render
  , eval
  , receiver: HE.input ChangeInput
  }

render :: State -> H.ComponentHTML Query
render st = st.input.wrapper
  [ MHH.input st.input.error $
      (if st.input.autofocus then (HP.autofocus true : _) else \x -> x)
        [ HP.type_ st.input.inputType
        , HP.value st.input.value
        , HP.name st.input.name
        , HP.id_ st.input.name
        , HE.onValueInput (HE.input ChangeValue)
        ]
  , HH.label [ HP.for st.input.name ] [ HH.text st.input.title ]
  , MHH.inputHelper st.input.description st.input.error
  ]

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  ChangeInput input next -> do
    H.modify_ (_ { input = input })
    pure next

  ChangeValue value next -> do
    H.raise value
    pure next
