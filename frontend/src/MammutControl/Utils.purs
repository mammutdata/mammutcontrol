module MammutControl.Utils
  ( redirect
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)

import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

redirect :: forall m. MonadEffect m => String -> m Unit
redirect hash = liftEffect $ window >>= location >>= setHash hash
