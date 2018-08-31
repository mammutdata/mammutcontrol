module MammutControl.Components.Common.SelectField
  ( component
  , Input(..)
  , defaultInput
  , Query(..)
  , Message
  ) where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Web.Event.Event (Event)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.HTMLHelpers as MHH

type Input =
  { value       :: String
  , name        :: String
  , title       :: String
  , description :: String
  , error       :: Maybe String
  , wrapper     :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
  , options     :: Array (Tuple String String)
  }

defaultInput :: Input
defaultInput =
  { value: ""
  , name: ""
  , title: ""
  , description: ""
  , error: Nothing
  , wrapper: MHH.inputWrapper
  , options: []
  }

type Message = String

type State = { input :: Input }

data Query a
  = ChangeInput Input a
  | ChangeValue Event a

component :: forall m. H.Component HH.HTML Query Input Message m
component = H.component
  { initialState: \input -> { input }
  , render
  , eval
  , receiver: HE.input ChangeInput
  }

render :: State -> H.ComponentHTML Query
render st = st.input.wrapper
  [ HH.div [HP.class_ (HH.ClassName "select-wrapper")]
      [ HH.select
          [ HP.name st.input.name
          , HP.id_ st.input.name
          , HE.onChange (HE.input ChangeValue)
          ]
          (map (\(Tuple value title) ->
                  HH.option [ HP.selected (value == st.input.value)
                            , HP.value value
                            ] [HH.text title])
               st.input.options)
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
    --H.raise value
    pure next
