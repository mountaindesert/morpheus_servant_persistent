module Options (Options(..), DbOptions(..), dbOptions, options, getOptions) where

import           Options.Applicative (Parser, ParserInfo, command,
                                      customExecParser, fullDesc, helper,
                                      hsubparser, info, prefs, progDesc,
                                      showHelpOnError, (<**>), (<|>))

data Options = Version | API | Config | DB DbOptions

data DbOptions = Migrate | ShowMigrations | Seed

dbOptions :: Parser Options
dbOptions =
  DB
    <$> hsubparser
      ( command "migrate" (info (pure Migrate) (progDesc "Migrate the database"))
          <> command "show" (info (pure ShowMigrations) (progDesc "Show the migrations"))
          <> command "seed" (info (pure Seed) (progDesc "Seed the database"))
      )

options :: Parser Options
options =
  hsubparser
    ( command "version" (info (pure Version) (progDesc "Print the API version"))
        <> command "api" (info (pure API) (progDesc "Run the API server"))
        <> command "config" (info (pure Config) (progDesc "Print the config"))
        <> command "db" (info dbOptions (progDesc "Database operations"))
    )
    <|> pure API

getOptions :: IO Options
getOptions = customExecParser (prefs showHelpOnError) opts
  where
    opts :: ParserInfo Options
    opts = info (options <**> helper) (fullDesc <> progDesc "Example of GraphQL API (using Morpheus) with Servant and PostgreSQL (using Esqueleto and Persistent)")
