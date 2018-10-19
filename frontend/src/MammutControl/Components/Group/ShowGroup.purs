module MammutControl.Components.Group.ShowGroup
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)

import Effect.Class (liftEffect)
import Effect.Console
import Effect.Aff (parallel, sequential)
import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.API.GroupAPI as API
import MammutControl.API.Helpers as API
import MammutControl.API.UserAPI as API
import MammutControl.API.WalletAPI as API
import MammutControl.Components.UI.MenuBar as MenuBar
import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes

type State =
  { groupID :: API.GroupID
  , group   :: Maybe API.Group
  , wallet  :: Maybe API.Wallet
  , users   :: Maybe (Array API.User)
  }

initialState :: API.GroupID -> State
initialState groupID =
  { groupID
  , group: Nothing
  , wallet: Nothing
  , users: Nothing
  }

data Query a
  = FetchGroup a
  | ChangeGroup API.GroupID a

component :: forall m. MonadAff m
          => H.Component HH.HTML Query API.GroupID Void m
component = H.lifecycleParentComponent
  { initialState: initialState
  , render
  , eval
  , receiver: HE.input ChangeGroup
  , initializer: Just (FetchGroup unit)
  , finalizer: Nothing
  }

render :: forall m. MonadAff m => State
       -> H.ParentHTML Query MenuBar.Query Unit m
render st =
  HH.div_
    [ HH.slot unit MenuBar.component { route: Groups } absurd
    , HH.div [HP.classes [MHH.container, MHH.section]]
        [ MHH.sectionTitle
            [ HH.text "Group "
            , HH.em_ [HH.text (maybe "" (\(API.Group g) -> g.name) st.group)]
            ]
        , case st.group of
            Nothing -> MHH.spinner
            Just group -> renderGroup st group
        ]
    ]

renderGroup :: forall m. MonadAff m => State -> API.Group
            -> H.ParentHTML Query MenuBar.Query Unit m
renderGroup st (API.Group group) = HH.div_
  [ HH.dl [HP.class_ (HH.ClassName "property-list")]
      [ HH.dt [HP.class_ (HH.ClassName "property-list__name")]
          [HH.text "Description"]
      , HH.dd [HP.class_ (HH.ClassName "property-list__value")]
          [ case group.description of
              Nothing -> HH.em_ [HH.text "No description given."]
              Just d  -> HH.text d
          ]
      , HH.dt [HP.class_ (HH.ClassName "property-list__name")]
          [HH.text "Associated wallet"]
      , HH.dd [HP.class_ (HH.ClassName "property-list__value")]
          [ case group.walletID of
              Nothing -> HH.em_ [HH.text "No wallet associated."]
              Just w -> case st.wallet of
                Nothing -> HH.text "A wallet is linked to this group."
                Just (API.Wallet wallet) ->
                  HH.a [HP.href ("#/wallets/" <> API.unWalletID wallet.id)]
                    [HH.text wallet.name]
          ]
      ]

  , MHH.sectionTitle2 [HH.text "Members"]
  , case st.users of
      Nothing -> MHH.spinner
      Just users -> MHH.collection (map (\(API.User u) -> HH.text (u.email)) users)
  ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query MenuBar.Query Unit Void m
eval = case _ of
    FetchGroup next -> do
      st <- H.get
      response <- H.liftAff $ API.getGroup st.groupID
      case response of
        Left err -> processError err
        Right group@(API.Group groupObj) -> do
          H.modify_ (_ { group = Just group })
          case groupObj.walletID of
            Nothing -> fetchUsers groupObj.id
            Just wid -> sequential $
              parallel (fetchWallet wid) *> parallel (fetchUsers groupObj.id)
      pure next

    ChangeGroup gid next -> do
      H.modify_ (_ { groupID = gid })
      eval $ FetchGroup next

  where
    fetchWallet :: forall m. MonadAff m => API.WalletID
                -> H.ParentDSL State Query MenuBar.Query Unit Void m Unit
    fetchWallet wid = do
      response <- H.liftAff $ API.getWallet wid
      case response of
        Left err -> processError err
        Right wallet -> H.modify_ (_ { wallet = Just wallet })

    fetchUsers :: forall m. MonadAff m => API.GroupID
               -> H.ParentDSL State Query MenuBar.Query Unit Void m Unit
    fetchUsers gid = do
      response <- H.liftAff $ API.getMembersOfGroup gid
      case response of
        Left err -> processError err
        Right users -> do
          liftEffect (log "hello")
          H.modify_ (_ { users = Just users })

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query MenuBar.Query Unit Void m Unit
processError = case _ of
  err -> liftEffect $ log $ show err