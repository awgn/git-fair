{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options
      ( Options(..)
      , parseOptions
      )
where

import           Options.Applicative

-- import Control.Applicative

data Options = Options
    { repository        :: Maybe String
    , branch            :: String
    , lastCommit        :: String
    , firstCommit       :: String
    , fileExtension     :: [String]
    , verbose           :: Bool
    , version           :: Bool
    , ignoreAllSpace    :: Bool
    } deriving (Show)


parseOptions :: Parser Options
parseOptions = do

      repository :: Maybe String <- optional
            (strOption
                  (long "repository" <> metavar "PATH" <> help "Specify the git repository (default current directory)")
            )

      branch <- strOption
            (long "branch" <> metavar "NAME" <> value "" <> help "Specify the branch to analyze (default is 'master')")

      lastCommit <- strOption
            (long "last-commit" <> short 'l' <> metavar "HASH" <> value "" <> help
                  "Specify the hash of the last-commit (default is 'HEAD')"
            )

      firstCommit <- strOption
            (long "first-commit" <> short 'f' <> metavar "HASH" <> value "" <> help
                  "Specify the hash of the first commit"
            )

      fileExtension <- many
            (strOption
                  (long "ext" <> short 'e' <> metavar "EXT" <> help
                        "Specify the file extension(s) to compute the statistics on"
                  )
            )

      ignoreAllSpace <- switch
            (long "ignore-all-space" <> short 'w' <> help "Ignore all while spaces in subsequent diff")

      verbose <- switch (long "verbose" <> short 'v' <> help "Enable verbose mode (debug)")

      version <- switch (long "version" <> short 'V' <> help "Print version")

      pure Options { .. }

