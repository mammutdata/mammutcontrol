module MammutControl.Time
  ( Time
  , getTime
  , runTime
  ) where

import Control.Eff
import Control.Eff.Lift
import Control.Monad.Base (MonadBase, liftBase)

import Data.Time

data Time a where
  GetTime :: Time UTCTime

getTime :: Member Time r => Eff r UTCTime
getTime = send GetTime

runTime :: (MonadBase IO m, SetMember Lift (Lift m) r)
        => Eff (Time ': r) a -> Eff r a
runTime = handle_relay return $ \action rest -> case action of
  GetTime -> do
    time <- lift $ liftBase getCurrentTime
    rest time
