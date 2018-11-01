module MammutControl.Components.Group.ShowGroup
  ( component
  , Query
  ) where

import Prelude

import Data.Array ((:), filter, null)
import Data.Either (Either(..))
import Data.Either.Nested
import Data.Functor.Coproduct.Nested
import Data.Maybe (Maybe(..), maybe)

import Effect.Class (liftEffect)
import Effect.Console
import Effect.Aff (parallel, sequential)
import Effect.Aff.Class (class MonadAff)

import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MammutControl.API.GroupAPI as API
import MammutControl.API.Helpers as API
import MammutControl.API.UserAPI as API
import MammutControl.API.WalletAPI as API
import MammutControl.Components.Common.InputField as InputField
import MammutControl.Components.UI.MenuBar as MenuBar
import MammutControl.HTMLHelpers as MHH
import MammutControl.Routes

type State =
  { error   :: forall p i. Maybe (HH.HTML p i)
  , groupID :: API.GroupID
  , group   :: Maybe API.Group
  , wallet  :: Maybe API.Wallet
  , users   :: Maybe (Array API.User)
  , email   :: String
  }

initialState :: API.GroupID -> State
initialState groupID =
  { error: Nothing
  , groupID
  , group: Nothing
  , wallet: Nothing
  , users: Nothing
  , email: ""
  }

data Query a
  = FetchGroup a
  | ChangeGroup API.GroupID a
  | RemoveMember API.UserID MouseEvent a
  | ChangeEmail String a
  | FormSubmitted Event a

type ChildQuery = Coproduct2 MenuBar.Query InputField.Query
type ChildSlot = Either2 Unit Unit

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
       -> H.ParentHTML Query ChildQuery ChildSlot m
render st =
  HH.div_
    [ HH.slot' CP.cp1 unit MenuBar.component { route: Groups } absurd
    , HH.div [HP.classes [MHH.container, MHH.section]]
        [ case st.error of
            Nothing -> HH.text ""
            Just errHTML -> MHH.errorCard [errHTML]
        , MHH.sectionTitle
            [ HH.text "Group "
            , HH.em_ [HH.text (maybe "" (\(API.Group g) -> g.name) st.group)]
            ]
        , case st.group of
            Nothing -> MHH.spinner
            Just group -> renderGroup st group
        ]
    ]

renderGroup :: forall m. MonadAff m => State -> API.Group
            -> H.ParentHTML Query ChildQuery ChildSlot m
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
      Just users -> MHH.collection (map renderUser users)
  , MHH.form [HE.onSubmit (HE.input FormSubmitted)]
      [ HH.slot' CP.cp2 unit InputField.component
          (InputField.defaultInput
            { name    = "email"
            , title   = "Email"
            , value   = st.email
            , wrapper = MHH.inputField6
            }) (HE.input ChangeEmail)
      , MHH.inputField6 [MHH.submitButton [] [HH.text "Add new member"]]
      ]
  ]

renderUser :: forall m. MonadAff m => API.User
           -> H.ParentHTML Query ChildQuery ChildSlot m
renderUser (API.User user) = HH.div_
  [ HH.text user.email
  , HH.a [ HP.class_ (HH.ClassName "secondary-content")
         , HE.onClick (HE.input (RemoveMember user.id))
         , HP.href "#"
         ]
      [MHH.icon "remove"]
  ]

eval :: forall m. MonadAff m
     => Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
eval = case _ of
    FetchGroup next -> do
      clearErrors
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

    RemoveMember uid event next -> do
      clearErrors
      liftEffect $ preventDefault $ toEvent event
      st <- H.get
      let users' = filter (\(API.User u) -> u.id /= uid) <$> st.users
      H.modify_ (_ { users = users' })
      removeMember st.groupID uid
      pure next

    ChangeEmail email next -> do
      H.modify_ (_ { email = email })
      pure next

    FormSubmitted event next -> do
      clearErrors
      liftEffect $ preventDefault event
      st <- H.get
      addMember st.groupID st.email
      pure next

  where
    clearErrors :: forall m. MonadAff m
                => H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
    clearErrors = H.modify_ (_ { error = Nothing })

    fetchWallet :: forall m. MonadAff m => API.WalletID
                -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
    fetchWallet wid = do
      response <- H.liftAff $ API.getWallet wid
      case response of
        Left _ -> pure unit
        Right wallet -> H.modify_ (_ { wallet = Just wallet })

    fetchUsers :: forall m. MonadAff m => API.GroupID
               -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
    fetchUsers gid = do
      response <- H.liftAff $ API.getMembersOfGroup gid
      case response of
        Left err -> processError err
        Right users -> H.modify_ (_ { users = Just users })

    removeMember :: forall m. MonadAff m => API.GroupID -> API.UserID
                 -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
    removeMember gid uid = do
      response <- H.liftAff $ API.removeMemberFromGroup gid uid
      case response of
        Left err -> do
          processError err
          fetchUsers gid
        Right users -> H.modify_ (_ { users = Just users })

    addMember :: forall m. MonadAff m => API.GroupID -> String
              -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
    addMember gid email = do
      response <- H.liftAff $ API.addMemberToGroup gid email
      case response of
        Left err -> processError err
        Right users -> H.modify_ (_ { email = "", users = Just users })

processError :: forall m. MonadAff m => API.APIError
             -> H.ParentDSL State Query ChildQuery ChildSlot Void m Unit
processError err = do
  let strs = API.humanReadableError err
  if null strs
    then pure unit
    else H.modify_ (_ { error = Just $ API.errorsHTML strs })
