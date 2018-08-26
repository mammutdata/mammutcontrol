module MammutControl.AccessControl.Class
  ( MonadAccessControl(..)
  ) where

import MammutControl.Data.Types

class MonadAccessControl m where
  skipAccessControl :: m a -> m a
  asUser :: UserID -> m a -> m a
