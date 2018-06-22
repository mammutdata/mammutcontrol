module MammutControl.Error
  ( ResourceType(..)
  , ValidationType(..)
  , MCError(..)
  , toServantErr
  , WithJSONErrors(..)
  ) where

import           Data.Aeson
import           Data.List.NonEmpty (NonEmpty(..), toList)
import           Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL

import           Servant
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication

data ResourceType
  = RTUser
  | RTGroup
  | RTReplica
  deriving Show

resourceTypeToText :: ResourceType -> T.Text
resourceTypeToText = \case
  RTUser    -> "user"
  RTGroup   -> "group"
  RTReplica -> "replica"

data ValidationType
  = CantBeEmpty
  | AlreadyTaken
  deriving Show

validationTypeToText :: ValidationType -> T.Text
validationTypeToText = \case
  CantBeEmpty  -> "cant_be_empty"
  AlreadyTaken -> "already_taken"

data MCError
  = ResourceNotFoundError ResourceType String
  | ValidationError (Maybe (T.Text, ValidationType)) String
  | AuthenticationError String
  | InternalError String
  | AccessDenied Bool String
  deriving Show

toServantErr :: NonEmpty MCError -> ServantErr
toServantErr (e :| []) = uncurry mkErr $ fmap SingleError $ toServantErr' e
toServantErr es =
  let errs = fmap toServantErr' es
      base :| bases = fmap fst errs
      base' | all (==base) bases = base
            | any ((>= 500) . errHTTPCode) (base:bases) = err500
            | otherwise = err400
  in mkErr base' $ MultipleErrors $ toList errs

toServantErr' :: MCError -> (ServantErr, SingleJSONError)
toServantErr' = \case
    ResourceNotFoundError resType _ ->
      let typeTxt = resourceTypeToText resType
      in (err404, SingleJSONError ("resource not found (" <> typeTxt <> ")")
                            Nothing (Just (typeTxt <> "_not_found")))
    ValidationError Nothing msg ->
      (err400, SingleJSONError (T.pack msg) Nothing (Just "validation_error"))
    ValidationError (Just (loc, typ)) msg ->
      (err400, SingleJSONError (T.pack msg) (Just loc)
                         (Just (validationTypeToText typ)))
    AuthenticationError msg ->
      (err401, SingleJSONError (T.pack msg) Nothing
                               (Just "authentication_error"))
    InternalError _ ->
      (err500, SingleJSONError "internal error" Nothing (Just "internal_error"))
    AccessDenied validToken msg ->
      let typ | validToken = "access_denied"
              | otherwise  = "invalid_token"
      in (err401, SingleJSONError (T.pack msg) Nothing (Just typ))

mkErr :: ServantErr -> JSONError -> ServantErr
mkErr base err = base
  { errHeaders = ("Content-Type", "application/json") : errHeaders base
  , errBody    = encode err
  }

data JSONError
  = SingleError SingleJSONError
  | MultipleErrors [(ServantErr, SingleJSONError)]

instance ToJSON JSONError where
  toJSON = \case
    SingleError err -> toJSON err
    MultipleErrors errs -> object
      [ "error"  .= ("multiple errors" :: T.Text)
      , "code"   .= ("multiple_errors" :: T.Text)
      , "errors" .= (flip map errs $ \(serr, err) ->
          case toJSON err of
            Object m -> Object $
              HM.insert "status_code" (toJSON (errHTTPCode serr)) m
            x -> x)
      ]

data SingleJSONError = SingleJSONError
  { jerrMessage  :: T.Text
  , jerrLocation :: Maybe T.Text
  , jerrCode     :: Maybe T.Text
  }

instance ToJSON SingleJSONError where
  toJSON SingleJSONError{..} = object $
    maybe id ((:) . ("location" .=)) jerrLocation
    . maybe id ((:) . ("code" .=)) jerrCode
    $ [ "error" .= jerrMessage ]

newtype WithJSONErrors a = WithJSONErrors a

instance HasServer a context => HasServer (WithJSONErrors a) context where
  type ServerT (WithJSONErrors a) m = ServerT a m

  route _ context delayed =
    routerWithJSONErrors $ route (Proxy :: Proxy a) context delayed

  hoistServerWithContext _ context nt server =
    hoistServerWithContext (Proxy :: Proxy a) context nt server

routerWithJSONErrors :: Router env -> Router env
routerWithJSONErrors = \case
    StaticRouter m fs ->
      StaticRouter (fmap routerWithJSONErrors m)
                   (map (fmap transformRoutingApplication) fs)
    CaptureRouter r -> CaptureRouter (routerWithJSONErrors r)
    CaptureAllRouter r -> CaptureAllRouter (routerWithJSONErrors r)
    RawRouter r -> RawRouter (fmap transformRoutingApplication r)
    Choice l r -> Choice (routerWithJSONErrors l) (routerWithJSONErrors r)

  where
    transformRoutingApplication :: RoutingApplication -> RoutingApplication
    transformRoutingApplication routingApp req f =
      routingApp req (f . transformRouteResult)

    transformRouteResult :: RouteResult a -> RouteResult a
    transformRouteResult = \case
      Fail err      -> Fail $ transformServantErr err
      FailFatal err -> FailFatal $ transformServantErr err
      Route x       -> Route x

    transformServantErr :: ServantErr -> ServantErr
    transformServantErr err@ServantErr{..} =
      case lookup "Content-Type" errHeaders of
        Nothing | not (BSL.null errBody) -> err
          { errHeaders = ("Content-Type", "application/json") : errHeaders
          , errBody    = encode $ SingleError $ SingleJSONError
                           (TE.decodeUtf8 . BSL.toStrict $ errBody)
                           Nothing Nothing
          }
        _ -> err
