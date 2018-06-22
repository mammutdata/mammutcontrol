module MammutControl.Components.UI.MenuBar
  ( component
  , Query
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))

import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes

foreign import data SideNavInstance :: Type
foreign import bindSideNav   :: Effect SideNavInstance
foreign import unbindSideNav :: SideNavInstance -> Effect Unit

type State =
  { route    :: Route
  , loggedIn :: Boolean
  , sidenav  :: Maybe SideNavInstance
  }

initialState :: Input -> State
initialState input =
  { route:    input.route
  , loggedIn: input.loggedIn
  , sidenav:  Nothing
  }

type Input =
  { route    :: Route
  , loggedIn :: Boolean
  }

data Query a
  = InputChanged Input a
  | BindSideNav a
  | UnbindSideNav a

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Void m
component = H.lifecycleComponent
  { initialState: initialState
  , render
  , eval
  , receiver: HE.input InputChanged
  , initializer: Just (BindSideNav unit)
  , finalizer: Just (UnbindSideNav unit)
  }

render :: State -> H.ComponentHTML Query
render st =
  let links
        | st.loggedIn =
          []
        | otherwise =
          [ HH.li
              (case st.route of
                 Signin -> [HP.class_ (HH.ClassName "active")]
                 _ -> [])
              [HH.a [HP.href "#/signin"] [HH.text "Sign in"]]
          , HH.li
              (case st.route of
                 Signup -> [HP.class_ (HH.ClassName "active")]
                 _ -> [])
              [HH.a [HP.href "#/signup"] [HH.text "Sign up"]]
          ]

      navBar = HH.nav_
        [ HH.div [HP.classes [HH.ClassName "nav-wrapper", MHH.container]]
            [ HH.a
                [HP.href "#/", HP.class_ (HH.ClassName "brand-logo")]
                [HH.text "Mammut"]

            , HH.ul [ HP.id_ "nav-mobile"
                    , HP.classes [ HH.ClassName "right"
                                 , HH.ClassName "hide-on-med-and-down"
                                 ]
                    ] links

            , HH.a [ HP.classes [ HH.ClassName "show-on-med-and-down"
                                , HH.ClassName "sidenav-trigger"
                                ]
                   , HP.attr (HH.AttrName "data-target") "slide-out"
                   , HP.href "#"
                   ]
                [HH.i [HP.class_ (HH.ClassName "material-icons")]
                      [HH.text "menu"]]
            ]
        ]

      sideNav =
        HH.ul [HP.id_ "slide-out", HP.class_ (HH.ClassName "sidenav")]
              links

  in HH.div_ [navBar, sideNav]

eval :: forall m. MonadAff m => Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  InputChanged input next -> do
    H.modify_ (_ { route = input.route, loggedIn = input.loggedIn })
    pure next

  BindSideNav next -> do
    inst <- liftEffect bindSideNav
    H.modify_ (_ { sidenav = Just inst })
    pure next

  UnbindSideNav next -> do
    st <- H.get
    case st.sidenav of
      Just inst -> liftEffect $ unbindSideNav inst
      Nothing -> pure unit
    pure next
