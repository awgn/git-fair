{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Options
      ( Options(..)
      , parseOptions
      )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>)
                                                , Semigroup(..)
                                                )

-- import Control.Applicative

data Options = Options
    {
      repository        :: String
    , branch            :: String
    , lastCommit        :: String
    , firstCommit       :: String
    , verbose           :: Bool
    , version           :: Bool
    , files             :: [String]
    } deriving (Show)


parseOptions :: Parser Options
parseOptions = do

      repository <- strOption
            (long "repository" <> metavar "PATH" <> value "." <> help
                  "Specify the git repository (default current directory)"
            )

      branch <- strOption
            (long "branch" <> metavar "NAME" <> value "master" <> help
                  "Specify the branch to analyze (default is 'master')"
            )

      lastCommit <- strOption
            (long "last-commit" <> short 'l' <> metavar "HASH" <> value "" <> help
                  "Specify the hash of the last-commit (default is 'HEAD')"
            )

      firstCommit <-
            strOption
                  (long "last-commit" <> short 'l' <> metavar "HASH" <> value "" <> help
                        "Specify the hash of the first commit"
                  )


      verbose <- switch (long "verbose" <> short 'v' <> help "Enable verbose mode (debug)")

      version <- switch (long "version" <> short 'V' <> help "Print version")

      files   <- some (argument str (metavar "FILES..."))
      pure Options { .. }

