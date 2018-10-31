module MammutControl.API.Helpers where

import Prelude

import Effect (Effect)
import Effect.Aff
import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.Array
import Data.Argonaut.Core as A
import Data.Either
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Show (class Show)
import Data.Tuple (Tuple(..))

import Foreign.Object as Obj

import Affjax
import Affjax.RequestBody
import Affjax.RequestHeader
import Affjax.ResponseFormat
import Affjax.StatusCode

import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

import Halogen.HTML as HH

import MammutControl.Session

apiRequest :: forall a. (Request a -> Request a) -> ResponseFormat a -> URL
           -> Aff (Response (Either ResponseFormatError a))
apiRequest f respFmt url = do
  mToken <- liftEffect getToken
  let req = defaultRequest { url = url, responseFormat = respFmt }
      req' = case mToken of
        Nothing -> req
        Just token ->
          let authorizationHeader =
                RequestHeader "Authorization" ("Bearer " <> token)
          in req { headers = authorizationHeader : req.headers }
  request $ f req'

apiGet :: forall a. ResponseFormat a -> URL
       -> Aff (Response (Either ResponseFormatError a))
apiGet = apiRequest \req -> req

apiPost :: forall a. ResponseFormat a -> URL -> RequestBody
        -> Aff (Response (Either ResponseFormatError a))
apiPost respFmt url body =
  let f req = req
        { method  = Left POST
        , content = Just body
        }
  in apiRequest f respFmt url

apiDelete :: forall a. ResponseFormat a -> URL
          -> Aff (Response (Either ResponseFormatError a))
apiDelete = apiRequest \req -> req { method = Left DELETE }

data APIErrorCode
  -- access control errors
  = AuthenticationError
  | InvalidToken
  | AccessDenied
  -- validation errors
  | ValidationError
  | CantBeEmpty
  | AlreadyTaken
  -- resource not found errors
  | UserNotFound
  | GroupNotFound
  | ReplicaNotFound
  -- constraint not satisfied
  | UnsatisfiedConstraintGroupHasMember
  -- general errors
  | InternalError
  | MultipleErrors

derive instance genericAPIErrorCode :: Generic APIErrorCode _

instance showAPIErrorCode :: Show APIErrorCode where
  show = genericShow

stringToAPIErrorCode :: String -> Maybe APIErrorCode
stringToAPIErrorCode = case _ of
  "authentication_error" -> Just AuthenticationError
  "invalid_token"        -> Just InvalidToken
  "access_denied"        -> Just AccessDenied
  "validation_error"     -> Just ValidationError
  "cant_be_empty"        -> Just CantBeEmpty
  "already_taken"        -> Just AlreadyTaken
  "user_not_found"       -> Just UserNotFound
  "group_not_found"      -> Just GroupNotFound
  "replica_not_found"    -> Just ReplicaNotFound
  "internal_error"       -> Just InternalError
  "multiple_errors"      -> Just MultipleErrors
  "constraint_group_has_member" -> Just UnsatisfiedConstraintGroupHasMember
  str -> Nothing

humanReadableErrorCode :: APIErrorCode -> String
humanReadableErrorCode = case _ of
  AuthenticationError ->
    "The authentication failed. Please check your credentials."
  InvalidToken        ->
    "The token is invalid or has expired. Please sign in again."
  AccessDenied        -> "Access denied."
  ValidationError     -> "The request is not valid."
  CantBeEmpty         -> "Can't be empty."
  AlreadyTaken        -> "Already taken."
  UserNotFound        -> "The user could not be found."
  GroupNotFound       -> "The group could not be found."
  ReplicaNotFound     -> "The replica could not be found."
  InternalError       -> "An internal error occured."
  MultipleErrors      -> "Multiple errors occured."
  UnsatisfiedConstraintGroupHasMember ->
    "The last member of a group can't be removed. Please delete the group instead."

humanReadableError :: APIError -> Array String
humanReadableError = case _ of
  APIError _ _ (Just _) _ _ -> []
  APIError _ _ _ (Just code) _ -> [humanReadableErrorCode code]
  APIError _ msg _ _ _ -> [msg]
  MultipleAPIErrors _ errs -> concatMap humanReadableError errs
  OtherError msg -> [msg]

errorsHTML :: forall p i. Array String -> HH.HTML p i
errorsHTML [err] = HH.text err
errorsHTML errs = HH.p_
  [ HH.text "Multiple errors were returned:"
  , HH.ul_ $ flip map errs $ \err -> HH.li_ [ HH.text err ]
  ]

data APIError
  = APIError StatusCode String (Maybe String)
             (Maybe APIErrorCode) (Maybe String)
  | MultipleAPIErrors StatusCode (Array APIError)
  | OtherError String

derive instance genericAPIError :: Generic APIError _

instance showAPIError :: Show APIError where
  show x = genericShow x

unreadableError :: StatusCode -> String -> APIError
unreadableError status msg =
  APIError status ("Unreadable error: " <> msg) Nothing Nothing Nothing

parseAPIErrorJSON :: StatusCode -> A.Json -> APIError
parseAPIErrorJSON status =
  A.caseJsonObject (unreadableError status "expected object") \obj ->
    case Obj.lookup "error" obj >>= A.toString of
      Nothing -> unreadableError status "missing key \"error\""
      Just msg ->
        let mLoc = do
              field <- Obj.lookup "location" obj
              A.toString field
            mCodeStr = do
              field <- Obj.lookup "code" obj
              A.toString field
            mCode = mCodeStr >>= stringToAPIErrorCode
        in case mCode of
             Just MultipleErrors -> parseMultipleErrors obj
             _ -> APIError status msg mLoc mCode mCodeStr

  where
    parseMultipleErrors :: Obj.Object A.Json -> APIError
    parseMultipleErrors obj =
      case Obj.lookup "errors" obj >>= A.toArray of
        Nothing -> unreadableError status "missing key \"errors\""
        Just values ->
          let errors = flip map values $ \value ->
                A.caseJsonObject (unreadableError status "expected object")
                   (\obj' ->
                      case Obj.lookup "status_code" obj' >>= A.toNumber of
                        Nothing -> unreadableError (StatusCode 999)
                                                   "missing key \"status_code\""
                        Just status' ->
                          parseAPIErrorJSON (StatusCode (round status'))
                                            value)
                   value
          in MultipleAPIErrors status errors

processResponse :: forall a. Response (Either ResponseFormatError A.Json)
                -> (Response A.Json -> Aff (Either String a))
                -> Aff (Either APIError a)
processResponse response f = do
  let StatusCode sc = response.status
  case response.body of
    Left formatError ->
      pure $ Left $ unreadableError response.status
                                    (printResponseFormatError formatError)
    Right body -> do
      if sc >= 400
        then do
          let err = parseAPIErrorJSON response.status body
          case err of
            APIError _ _ _ (Just InvalidToken) _ -> liftEffect $ do
              clearToken
              setHref "/" =<< location =<< window
            _ -> pure unit
          liftEffect $ logError err
          pure $ Left err

        else do
          res <- f response { body = body }
          pure $ case res of
            Left err -> Left $ OtherError err
            Right x  -> Right x

logError :: APIError -> Effect Unit
logError = case _ of
  APIError (StatusCode status) msg mLoc _ mCodeStr ->
    log $ "API error (" <> show status <> "): " <> msg
      <> maybe "" (" at location " <> _) mLoc
      <> maybe "" (" with code " <> _) mCodeStr
  MultipleAPIErrors (StatusCode status) errs -> do
    log $ "API error (" <> show status <> "): multiple errors"
    for_ errs logError
  OtherError str -> log str
