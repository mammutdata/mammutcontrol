module MammutControl.HTMLHelpers
  ( container
  , section
  , inputField
  , row
  , row_
  , col12
  , col12_
  , col6
  , col6_
  , centeredDiv
  , form
  , inputField12
  , inputField6
  , inputWrapper
  , input
  , inputNoValidate
  , submitButton
  , inputHelper
  , errorCard
  ) where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)

import DOM.HTML.Indexed as H

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

container :: HH.ClassName
container = HH.ClassName "container"

section :: HH.ClassName
section = HH.ClassName "section"

col :: HH.ClassName
col = HH.ClassName "col"

s12 :: HH.ClassName
s12 = HH.ClassName "s12"

s6 :: HH.ClassName
s6 = HH.ClassName "s6"

m12 :: HH.ClassName
m12 = HH.ClassName "m12"

l6 :: HH.ClassName
l6 = HH.ClassName "l6"

btn :: HH.ClassName
btn = HH.ClassName "btn"

inputField :: HH.ClassName
inputField = HH.ClassName "input-field"

wavesEffect :: HH.ClassName
wavesEffect = HH.ClassName "waves-effect"

wavesLight :: HH.ClassName
wavesLight = HH.ClassName "waves-light"

row :: forall p i. HH.Node H.HTMLdiv p i
row props children = HH.div (HP.class_ (HH.ClassName "row") : props) children

row_ :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
row_ = row []

col12 :: forall p i. HH.Node H.HTMLdiv p i
col12 props children = HH.div (HP.classes [col, s12] : props) children

col12_ :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
col12_ = col12 []

col6 :: forall p i. HH.Node H.HTMLdiv p i
col6 props children = HH.div (HP.classes [col, s6] : props) children

col6_ :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
col6_ = col6 []

centeredDiv :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
centeredDiv children = row_
  [ HH.div [HP.classes [col, HH.ClassName "s12 m8 push-m2 l6 push-l3"]]
           children
  ]

form :: forall p i. HH.Node H.HTMLform p i
form props children = row_ [ HH.form (HP.classes [col, s12] : props) children ]

inputField12 :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
inputField12 = HH.div [HP.classes [col, s12, inputField]]

inputField6 :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
inputField6 = HH.div [HP.classes [col, l6, m12, s12, inputField]]

inputWrapper :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
inputWrapper children = row_ [inputField12 children]

input :: forall a p i. Maybe a -> HH.Leaf H.HTMLinput p i
input mErr props =
  HH.input (HP.class_ (HH.ClassName (maybe "validate" (const "invalid") mErr))
            : props)

inputNoValidate :: forall a p i. String -> Maybe a -> HH.Leaf H.HTMLinput p i
inputNoValidate val mErr props =
  HH.input (HP.class_ (HH.ClassName (maybe (if val == "" then "" else "valid")
                                           (const "invalid") mErr))
            : props)

submitButton :: forall p i. HH.Node H.HTMLbutton p i
submitButton props children =
  HH.button
    (HP.classes [btn, wavesEffect, wavesLight]
     : HP.type_ HP.ButtonSubmit
     : props)
    children

inputHelper :: forall p i. String -> Maybe String -> HH.HTML p i
inputHelper helperTxt mErr =
  HH.span
    [ HP.class_ (HH.ClassName "helper-text")
    , HP.attr (HH.AttrName "data-error") (fromMaybe "" mErr)
    ]
    [ HH.text helperTxt ]

errorCard :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
errorCard children =
  row_
    [ col12_
        [ HH.div [HP.classes [ HH.ClassName "card-panel"
                             , HH.ClassName "red" ]]
                 children
        ]
    ]