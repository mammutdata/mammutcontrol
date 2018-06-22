module MammutControl.Server where

import System.IO

import Network.Wai.Handler.Warp

import MammutControl.API
import MammutControl.Config
import MammutControl.Options

run :: IO ()
run = do
  opts   <- getOptions
  config <- readConfigOrDie $ optsConfigPath opts

  let port = configPort config
  pool <- mkConnectionPool config

  let settings =
        setBeforeMainLoop (hPutStrLn stderr $
                            "Server listening on port " ++ show port) $
        setPort port defaultSettings

  app <- api pool
  runSettings settings app
