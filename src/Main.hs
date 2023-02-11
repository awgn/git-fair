{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

import Options.Applicative
    ( header, fullDesc, helper, info, execParser )
import Data.List ( findIndex )
import Data.Tuple.Select ( Sel3(sel3), Sel2(sel2), Sel1(sel1) )

import Options ( parseOptions, Options(..) )
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import Data.Text.Read ( decimal )
import TextShow ( TextShow(showt) )

import System.Process
    ( waitForProcess,
      proc,
      createProcess,
      StdStream(CreatePipe),
      CreateProcess(cwd, std_out) )
import System.FilePath.Posix ( isExtensionOf )
import System.IO ( stdout, hFlush )
import Control.Monad ( void )

import qualified Data.Map.Strict  as M

import qualified Paths_GitFair  as P
import Data.Version ( showVersion )

main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> parseOptions) (fullDesc <> header "git-fair: git statistical tool")
        main' :: Options -> IO ()
        main' o@Options {..} | version   = putStrLn $ showVersion P.version
                             | otherwise = runStat o

data CommitInfo = CommitInfo
    { author       :: !T.Text
    , commitHash   :: !T.Text
    , parentHashes :: ![T.Text]
    } deriving stock (Show,Eq)


data CommitStat = CommitStat
    { commit     :: {-# UNPACK #-} !CommitInfo
    , insertions :: {-# UNPACK #-} !Int
    , deletions  :: {-# UNPACK #-} !Int
    } deriving stock (Show,Eq)


runStat :: Options -> IO ()
runStat opt@Options {..} = do
  cs <- cropCommits opt <$> gitCommitInfo opt
  ss <- mapM (gitCommitStat opt) cs

  let aggr  = aggregateStat ss
      total = totalStats aggr

  if null branch
    then T.putStrLn "\nGit fair-stat (current branch):"
    else T.putStrLn $ "\nGit fair-stat (" <> showt branch <> "):"

  printStat $ aggregateStat ss
  T.putStrLn
    $ "\n   TOTAL : insertions:" <> showt (sel1 total) <> ", deletions: "
    <> showt (sel2 total) <> ", commits: " <> showt (sel3 total)


mkCommitInfo :: T.Text -> CommitInfo
mkCommitInfo t =
  case T.splitOn "|" t of
    [author, commitHash, ps] -> let parentHashes = T.splitOn " " ps in CommitInfo{..}
    _ -> error "mkCommitInfo: invalid commit info"


gitCommitInfo :: Options -> IO [CommitInfo]
gitCommitInfo Options {..} = do
  (_, Just hout, _, ph) <- createProcess
    (proc "git" (["log", "--first-parent", "--pretty=%an|%H|%P"] <>
      [ branch | (not . null) branch ])) { std_out = CreatePipe
                                         , cwd = repository
                                         }
  ret <- fmap mkCommitInfo . T.lines <$> T.hGetContents hout
  void $ waitForProcess ph
  return ret

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
parseLine' xs =
  let res = do
        (a, xs') <- decimal xs
        (d, ys) <- decimal (T.strip xs')
        return (a, d, T.strip ys)
  in case res of
      Right (a, d, ys) -> (a, d, ys)
      _ -> (0, 0, "")


gitCommitStat :: Options -> CommitInfo -> IO CommitStat
gitCommitStat Options {..} com = case parentHashes com of
  ("" : _) -> gitCommitSingleStat Options { .. } com
  []       -> gitCommitSingleStat Options { .. } com
  _        -> gitCommitDeltaStat Options { .. } com


gitCommitDeltaStat :: Options -> CommitInfo -> IO CommitStat
gitCommitDeltaStat Options {..} com = do
  (_, Just hout, _, ph) <-
    createProcess (proc "git" ( ["diff", "--numstat"]
                                <> [ "-w" | ignoreAllSpace ]
                                <> [(T.unpack . head . parentHashes) com, (T.unpack . commitHash) com])) { std_out = CreatePipe, cwd = repository }

  (add, del, _) <- unzip3 . fmap (parseLine fileExtension) . T.lines <$> T.hGetContents hout

  let totalAdd = sum add
      totalDel = sum del

  void $ waitForProcess ph

  if verbose
    then
      T.putStrLn
      $  " [" <> author com <> "] "
      <> showt (parentHashes com)
      <> " -> " <> commitHash com <> " | insertions:" <> showt totalAdd <> " deletions: " <> showt totalDel
      <> if (totalAdd + totalDel) == 0 then " (SKIPPED)" else ""
    else putChar (mkIcon (totalAdd + totalDel) (author com)) >> hFlush stdout

  return CommitStat { commit = com, insertions = sum add, deletions = sum del }


gitCommitSingleStat :: Options -> CommitInfo -> IO CommitStat
gitCommitSingleStat Options {..} com = do
  (_, Just hout, _, _) <- createProcess (
    proc "git" (["log", "--oneline", "--numstat"] <> [(T.unpack . commitHash) com])
      )
    { std_out = CreatePipe
    , cwd     = repository
    }

  (add, del, _) <- unzip3 . fmap (parseLine fileExtension) . tail . T.lines <$> T.hGetContents hout

  let totalAdd = sum add
      totalDel = sum del
  if verbose
    then
      T.putStrLn
      $  " [" <> author com <> "] " <> commitHash com <> " | insertions:" <> showt totalAdd <> " deletions: "
      <> showt totalDel
      <> if (totalAdd + totalDel) == 0 then " (SKIPPED)" else ""
    else putChar (mkIcon (totalAdd + totalDel) (author com)) >> hFlush stdout

  return CommitStat { commit = com, insertions = sum add, deletions = sum del }


sumStat :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sumStat (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)
{-# INLINE sumStat #-}


aggregateStat :: [CommitStat] -> M.Map T.Text (Int, Int, Int)
aggregateStat =
  foldr (\st m -> M.insertWith sumStat ((author . commit) st) (insertions st, deletions st, 1) m) M.empty
{-# INLINE aggregateStat #-}


totalStats :: M.Map T.Text (Int, Int, Int) -> (Int, Int, Int)
totalStats = foldr sumStat (0, 0, 0)
{-# INLINE totalStats #-}


printStat :: M.Map T.Text (Int, Int, Int) -> IO ()
printStat m = void $ M.traverseWithKey
  (\k v ->
    T.putStrLn
      $  "   author: " <> k <> " -> " <> "insertions:" <> showt (sel1 v)
        <> ", deletions: " <> showt (sel2 v) <> ", commits: " <> showt (sel3 v)) m


mayDrop :: Maybe Int -> [a] -> [a]
mayDrop Nothing  x = x
mayDrop (Just n) x = drop n x
{-# INLINE mayDrop #-}

mayTake :: Maybe Int -> [a] -> [a]
mayTake Nothing  x = x
mayTake (Just n) x = take n x
{-# INLINE mayTake #-}

mkIcon :: Int -> T.Text -> Char
mkIcon 0 = const '.'
mkIcon _ = T.head
{-# INLINE mkIcon #-}
