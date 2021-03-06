module MammutControl.Components.UI.MenuBar
  ( component
  , Query
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..), isJust)

import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes
import MammutControl.Session (getToken, clearToken)

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
  , loggedIn: false
  , sidenav:  Nothing
  }

type Input = { route :: Route }

data Query a
  = InputChanged Input a
  | BindSideNav a
  | UnbindSideNav a
  | Init a
  | Signout a

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Void m
component = H.lifecycleComponent
  { initialState: initialState
  , render
  , eval
  , receiver: HE.input InputChanged
  , initializer: Just (Init unit)
  , finalizer: Just (UnbindSideNav unit)
  }

render :: State -> H.ComponentHTML Query
render st =
  let links
        | st.loggedIn =
          [ HH.li
              (case sectionFromRoute st.route of
                 Just GroupSection -> [HP.class_ (HH.ClassName "active")]
                 _ -> [])
              [HH.a [HP.href "#/groups"] [HH.text "Groups"]]
          , HH.li_ [HH.a [HP.href "#", HE.onClick (HE.input_ Signout)]
                         [HH.text "Sign out"]]
          ]
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

      navBar = HH.nav [HP.class_ (HH.ClassName "teal")]
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
    H.modify_ (_ { route = input.route })
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

  Init next -> do
    mToken <- liftEffect getToken
    H.modify_ (_ { loggedIn = isJust mToken })
    eval $ BindSideNav next

  Signout next -> do
    liftEffect do
      clearToken
      setHref "/" =<< location =<< window
    pure next
