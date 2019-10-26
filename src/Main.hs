{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Options.Applicative
import           Data.Version                   ( showVersion )
import           Data.List
import           Data.Tuple.Select

import qualified Paths_GitFair                 as P
import           Options
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text.Read
import           System.Process
import           System.FilePath.Posix
import           Control.Monad

import           SplitGroups

import qualified Data.Map.Strict               as M


main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> parseOptions) (fullDesc <> header "git-fair: git statistical tool")


main' :: Options -> IO ()
main' o@Options {..} | version   = putStrLn $ showVersion P.version
                     | otherwise = runStat o


data CommitInfo = CommitInfo
    { commitHash :: T.Text
    , author     :: T.Text
    } deriving (Show,Eq)

data StatCommit = StatCommit
    { commit    :: CommitInfo
    , addition  :: Int
    , deletion  :: Int
    } deriving (Show,Eq)


runStat :: Options -> IO ()
runStat opt@Options {..} = do
  cs <- cropCommits opt <$> gitCommitInfo opt
  ds <- mapM (gitCommitDeltaStat opt) (spanGroups 2 cs)

  ss <- if exclude1stcommit then return ds else gitCommitStat opt (last cs) >>= \c -> return (ds <> [c])

  let aggr  = aggregateStat ss
      total = totalStats aggr

  putStrLn $ "Git fair-stat:"
  putStrLn $ "  total addition: " <> show (sel1 total) <> ", deletion: " <> show (sel2 total) <> ", commits: " <> show
    (sel3 total)
  printStat $ aggregateStat ss


mkCommitInfo :: T.Text -> CommitInfo
mkCommitInfo t = let [h, a] = T.splitOn "|" t in CommitInfo h a


gitCommitInfo :: Options -> IO [CommitInfo]
gitCommitInfo Options {..} = do
  (_, Just hout, _, ph) <- createProcess (proc "git" ["log", "--pretty=%H|%an"]) { std_out = CreatePipe
                                                                                 , cwd     = repository
                                                                                 }
  ret <- fmap mkCommitInfo <$> (return . T.lines =<< T.hGetContents hout)
  void $ waitForProcess ph
  return ret

mayDrop :: Maybe Int -> [a] -> [a]
mayDrop Nothing  x = x
mayDrop (Just n) x = drop n x

mayTake :: Maybe Int -> [a] -> [a]
mayTake Nothing  x = x
mayTake (Just n) x = take n x


cropCommits :: Options -> [CommitInfo] -> [CommitInfo]
cropCommits Options {..} cs = case (T.pack firstCommit, T.pack lastCommit) of
  ("", "") -> cs
  (f , "") -> mayTake ((+ 1) <$> findIndex (\c -> f `T.isPrefixOf` commitHash c) cs) cs
  ("", l ) -> mayDrop (findIndex (\c -> l `T.isPrefixOf` commitHash c) cs) cs
  (f, l) ->
    let tmp = mayTake ((+ 1) <$> findIndex (\c -> f `T.isPrefixOf` commitHash c) cs) cs
    in  mayDrop (findIndex (\c -> l `T.isPrefixOf` commitHash c) tmp) tmp

parseLine :: [String] -> T.Text -> (Int, Int, T.Text)
parseLine [] xs = parseLine' xs
parseLine ext xs =
  let (a, d, name) = parseLine' xs
  in  if any (\e -> e `isExtensionOf` T.unpack name) ext then (a, d, name) else (0, 0, name)


parseLine' :: T.Text -> (Int, Int, T.Text)
parseLine' xs = case decimal xs of
  Right (a, xs') -> case decimal (T.strip xs') of
    Right (d, ys) -> (a, d, T.strip ys)
    _             -> (0, 0, "")
  _ -> (0, 0, "")


gitCommitDeltaStat :: Options -> [CommitInfo] -> IO StatCommit
gitCommitDeltaStat Options {..} [cur, prev] = do

  (_, Just hout, _, ph) <- createProcess (proc
                                           "git"
                                           (  ["diff", "--numstat"]
                                           <> (if ignoreAllSpace then ["-w"] else [])
                                           <> [(T.unpack . commitHash) prev, (T.unpack . commitHash) cur]
                                           )
                                         )
    { std_out = CreatePipe
    , cwd     = repository
    }

  (add, del, _) <- unzip3 . fmap (parseLine fileExtension) <$> (return . T.lines =<< T.hGetContents hout)

  void $ waitForProcess ph

  when verbose
    $  T.putStrLn
    $  " ["
    <> author cur
    <> "] "
    <> commitHash prev
    <> " -> "
    <> commitHash cur
    <> " | addition: "
    <> tshow (sum add)
    <> " deletion: "
    <> tshow (sum del)

  return StatCommit { commit = cur, addition = sum add, deletion = sum del }

gitCommitDeltaStat Options {..} [cur] = return StatCommit { commit = cur, addition = 0, deletion = 0 }
gitCommitDeltaStat Options {..} _     = errorWithoutStackTrace $ "git-fair: unexpected delta commit statistics"


gitCommitStat :: Options -> CommitInfo -> IO StatCommit
gitCommitStat Options {..} com = do
  (_, Just hout, _, _) <- createProcess (proc "git" (["log", "--oneline", "--numstat"] <> [(T.unpack . commitHash) com])
                                        )
    { std_out = CreatePipe
    , cwd     = repository
    }

  (add, del, _) <- unzip3 . fmap (parseLine fileExtension) . tail <$> (return . T.lines =<< T.hGetContents hout)

  when verbose
    $  T.putStrLn
    $  " ["
    <> author com
    <> "] "
    <> commitHash com
    <> " | addition: "
    <> tshow (sum add)
    <> " deletion: "
    <> tshow (sum del)

  return StatCommit { commit = com, addition = sum add, deletion = sum del }


sumStat :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sumStat (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)


aggregateStat :: [StatCommit] -> M.Map T.Text (Int, Int, Int)
aggregateStat ss =
  foldr (\st m -> M.insertWith sumStat ((author . commit) st) (addition st, deletion st, 1) m) M.empty ss


totalStats :: M.Map T.Text (Int, Int, Int) -> (Int, Int, Int)
totalStats m = foldr (\s v -> sumStat s v) (0, 0, 0) m


printStat :: M.Map T.Text (Int, Int, Int) -> IO ()
printStat m = void $ M.traverseWithKey
  (\k v ->
    T.putStrLn
      $  "   author: "
      <> k
      <> " -> "
      <> "addition: "
      <> tshow (sel1 v)
      <> ", deletion: "
      <> tshow (sel2 v)
      <> ", commits: "
      <> tshow (sel3 v)
  )
  m

tshow :: Show a => a -> T.Text
tshow = T.pack . show
