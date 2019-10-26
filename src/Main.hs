{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Options.Applicative
import           Data.Version                   ( showVersion )

import qualified Paths_GitFair                 as P
import           Options


main :: IO ()
main = execParser opts >>= main'
    where opts = info (helper <*> parseOptions) (fullDesc <> header "git-fair: git statistical tool")


main' :: Options -> IO ()
main' Options {..} | version   = putStrLn $ showVersion P.version
                   | otherwise = putStrLn $ "Done"
