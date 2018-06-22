-- | Shameless copy of https://github.com/parsonsmatt/servant-persistent/blob/master/src/DevelMain.hs

module MammutControl.DevelServer
  ( reload
  ) where

import GHC.Word             (Word32)

import Control.Concurrent   ( MVar, ThreadId, forkIO, killThread
                            , newEmptyMVar, putMVar, takeMVar )
import Control.Exception    (finally)

import Data.IORef           (IORef, newIORef, readIORef, writeIORef)

import Foreign.Store        ( Store(..), lookupStore, readStore
                            , storeAction, withStore )

import MammutControl.Server (run)

-- | Start or reload the server.
reload :: IO ()
reload = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- No server running.
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    -- Server is already running.
    Just tidStore -> restartAppInNewThread tidStore

doneStore :: Store (MVar ())
doneStore = Store 0

-- Shut the server down with killThread and wait for the done signal.
restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
  killThread tid
  withStore doneStore takeMVar
  readStore doneStore >>= start

-- Start the server in a separate thread.
start :: MVar () -- ^ Written to when the thread is killed.
      -> IO ThreadId
start done = forkIO $
  finally run
          -- Note that this implies concurrency
          -- between shutdownApp and the next app that is starting.
          -- Normally this should be fine
          (putMVar done ())

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref
