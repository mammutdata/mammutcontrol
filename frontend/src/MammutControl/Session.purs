module MammutControl.Session
  ( getToken
  , setToken
  , clearToken
  ) where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe)

import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

getToken :: Effect (Maybe String)
getToken = do
  st <- getStorage
  getItem "token" st

setToken :: String -> Effect Unit
setToken token = do
  st <- getStorage
  setItem "token" token st

clearToken :: Effect Unit
clearToken = do
  st <- getStorage
  removeItem "token" st

getStorage :: Effect Storage
getStorage = localStorage =<< window
