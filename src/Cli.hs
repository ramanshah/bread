module Cli where

import Options.Applicative

data Config = Config
  { filePath    :: String
  , scaleFactor :: Float
  , outputYAML  :: Bool }

config :: Parser Config
config = Config
  <$> argument str
      ( metavar "YAML_FILE_PATH" )
  <*> argument auto
      ( metavar "SCALE_FACTOR" )
  <*> switch
      ( long "yaml"
     <> short 'y'
     <> help "Whether to output recipe as YAML instead of a formatted table" )

cliOpts :: ParserInfo Config
cliOpts = info (helper <*> config)
   ( fullDesc
  <> progDesc "Print scaled recipe from YAML_FILE_PATH to the console"
  <> header "bread - scale recipes at the command line" )
