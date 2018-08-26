module MammutControl.Options
  ( Options(..)
  , getOptions
  ) where

import Options.Applicative

newtype Options = Options { optsConfigPath :: FilePath }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (short 'c' <> long "config" <> metavar "PATH"
                 <> help "Path to the config file" <> value "config.yml")

getOptions :: IO Options
getOptions = execParser $ info (helper <*> optionsParser) $
  fullDesc <> progDesc "Web server for https://api.mammutdata.com"
