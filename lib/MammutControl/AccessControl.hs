module MammutControl.AccessControl
  ( checkAccess
  ) where

import Control.Eff
import Control.Eff.Exception (Exc, throwError)

import MammutControl.Data.User
import MammutControl.Error

checkAccess :: (Member (Exc MCError) r, Member UserAccess r)
            => Maybe UserID -> Eff r a -> Eff r a
checkAccess muid = checkUserAccess muid

checkUserAccess :: (Member (Exc MCError) r, Member UserAccess r)
                => Maybe UserID -> Eff r a -> Eff r a
checkUserAccess muid = interpose return $ \action rest -> case action of
  HashPassword _ -> send action >>= rest
  CreateUser _ -> send action >>= rest
  GetUser uid
    | muid == Just uid -> send action >>= rest
    | otherwise -> deny $ "can't get user with ID " ++ show (unUserID uid)
  GetUserByEmail _ -> send action >>= rest -- needed to sign in
  EditUser uid _
    | muid == Just uid -> send action >>= rest
    | otherwise -> deny $ "can't edit user with ID " ++ show (unUserID uid)
  DeleteUser uid
    | muid == Just uid -> send action >>= rest
    | otherwise -> deny $ "can't delete user with ID " ++ show (unUserID uid)

deny :: Member (Exc MCError) r => String -> Eff r a
deny = throwError . AccessDenied
