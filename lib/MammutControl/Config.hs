module MammutControl.Config
  ( Config(..)
  , readConfigOrDie
  , mkConnectionPool
  ) where

import           Data.Pool (Pool, createPool)
import           Data.Yaml
import qualified Data.ByteString as BS

import           System.Exit
import           System.IO

import           Database.PostgreSQL.Simple ( ConnectInfo(..), Connection
                                            , connect, close )

data Config = Config
  { configPort   :: Int
  , configDBInfo :: ConnectInfo
  }

instance FromJSON Config where
  parseJSON = withObject "config" $ \obj -> do
    configPort <- obj .: "port"

    db <- obj .: "database"
    configDBInfo <- ConnectInfo
      <$> db .: "hostname"
      <*> db .: "port"
      <*> db .: "username"
      <*> db .: "password"
      <*> db .: "name"

    return Config{..}

readConfigOrDie :: FilePath -> IO Config
readConfigOrDie path = do
  contents <- BS.readFile path
  case decodeEither contents of
    Left err -> do
      hPutStrLn stderr $ "Can't read config: " ++ err
      exitFailure
    Right config -> return config

mkConnectionPool :: Config -> IO (Pool Connection)
mkConnectionPool config =
  createPool (connect (configDBInfo config)) close 1 3600 5
