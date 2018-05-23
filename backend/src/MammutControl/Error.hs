module MammutControl.Error
  ( MCError(..)
  , toServantErr
  ) where

import Servant.Server

data MCError
  = ResourceNotFoundError String
  | ValidationError (Maybe String) String
  | AuthenticationError String
  | InternalError String
  | AccessDenied String
  deriving Show

toServantErr :: MCError -> ServantErr
toServantErr = \case
  ResourceNotFoundError _ -> err404
  ValidationError _ _     -> err400
  AuthenticationError _   -> err401
  InternalError _         -> err500
  AccessDenied _          -> err401
