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
import Data.HTTP.Method (Method(..))
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Foreign.Object as Obj

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request as RQ
import Network.HTTP.Affjax.Response as RS
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode

import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

import Halogen.HTML as HH

import MammutControl.Session

get' :: forall a. RS.Response a -> URL -> Affjax a
get' response url = do
  mToken <- liftEffect getToken
  let request = defaultRequest { url = url }
      request' = case mToken of
        Nothing -> request
        Just token ->
          let authorizationHeader =
                RequestHeader "Authorization" ("Bearer " <> token)
          in request { headers = authorizationHeader :  request.headers }
  affjax response request'

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
  -- general errors
  | InternalError
  | MultipleErrors

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
  str                    -> Nothing

humanReadableError :: APIErrorCode -> String
humanReadableError = case _ of
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

humanReadableErrorList :: forall p i. Array String -> HH.HTML p i
humanReadableErrorList [err] = HH.text err
humanReadableErrorList errs = HH.p_
  [ HH.text "Multiple errors were returned:"
  , HH.ul_ $ flip map errs $ \err -> HH.li_ [ HH.text err ]
  ]

data APIError
  = APIError StatusCode String (Maybe String)
             (Maybe APIErrorCode) (Maybe String)
  | MultipleAPIErrors StatusCode (Array APIError)
  | OtherError String

unreadableError :: StatusCode -> APIError
unreadableError status =
  APIError status "unreadable error" Nothing Nothing Nothing

parseAPIErrorJSON :: StatusCode -> A.Json -> APIError
parseAPIErrorJSON status = A.caseJsonObject (unreadableError status) \obj ->
    case Obj.lookup "error" obj >>= A.toString of
      Nothing -> unreadableError status
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
        Nothing -> unreadableError status
        Just values ->
          let errors = flip map values $ \value ->
                A.caseJsonObject (unreadableError status)
                   (\obj' ->
                      case Obj.lookup "status_code" obj' >>= A.toNumber of
                        Nothing -> unreadableError $ StatusCode 999
                        Just status' ->
                          parseAPIErrorJSON (StatusCode (round status'))
                                            value)
                   value
          in MultipleAPIErrors status errors


processResponse :: forall a. AffjaxResponse A.Json -> Aff (Either String a)
                -> Aff (Either APIError a)
processResponse response action = do
  let StatusCode sc = response.status
  if sc >= 400
    then do
      let err = parseAPIErrorJSON response.status response.response
      case err of
        APIError _ _ _ (Just InvalidToken) _ -> liftEffect $ do
          clearToken
          setHref "/" =<< location =<< window
        _ -> pure unit
      liftEffect $ logError err
      pure $ Left err

    else do
      eRes <- action
      pure $ case eRes of
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
