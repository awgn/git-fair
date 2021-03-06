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
import           System.IO
import           Control.Monad

import qualified Data.Map.Strict               as M


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
    } deriving (Show,Eq)

data CommitStat = CommitStat
    { commit     :: {-# UNPACK #-} !CommitInfo
    , insertions :: {-# UNPACK #-} !Int
    , deletions  :: {-# UNPACK #-} !Int
    } deriving (Show,Eq)


runStat :: Options -> IO ()
runStat opt@Options {..} = do
  cs <- cropCommits opt <$> gitCommitInfo opt
  ss <- mapM (gitCommitStat opt) cs

  let aggr  = aggregateStat ss
      total = totalStats aggr

  if null branch then putStrLn "\nGit fair-stat (current branch):" else putStrLn $ "\nGit fair-stat (" <> branch <> "):"
  printStat $ aggregateStat ss
  putStrLn
    $  "\n   TOTAL : insertions:"
    <> show (sel1 total)
    <> ", deletions: "
    <> show (sel2 total)
    <> ", commits: "
    <> show (sel3 total)


mkCommitInfo :: T.Text -> CommitInfo
mkCommitInfo t = let [author, commitHash, ps] = T.splitOn "|" t 
                     parentHashes = T.splitOn " " ps
                      in CommitInfo{..}


gitCommitInfo :: Options -> IO [CommitInfo]
gitCommitInfo Options {..} = do
  (_, Just hout, _, ph) <- createProcess
    (proc "git" (["log", "--first-parent", "--pretty=%an|%H|%P"] <> [ branch | (not . null) branch ])) { std_out = CreatePipe
                                                                                                       , cwd = repository
                                                                                                       }
  ret <- fmap mkCommitInfo . T.lines <$> T.hGetContents hout
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


mkIcon :: Int -> T.Text -> Char
mkIcon 0 = const '.'
mkIcon _ = T.head


gitCommitStat :: Options -> CommitInfo -> IO CommitStat
gitCommitStat Options {..} com = case parentHashes com of
  ("" : _) -> gitCommitSingleStat Options { .. } com
  []       -> gitCommitSingleStat Options { .. } com
  _        -> gitCommitDeltaStat Options { .. } com


gitCommitDeltaStat :: Options -> CommitInfo -> IO CommitStat
gitCommitDeltaStat Options {..} com = do
  (_, Just hout, _, ph) <- createProcess (proc
                                           "git"
                                           (  ["diff", "--numstat"]
                                           <> [ "-w" | ignoreAllSpace ]
                                           <> [(T.unpack . head . parentHashes) com, (T.unpack . commitHash) com]
                                           )
                                         ) { std_out = CreatePipe
                                           , cwd     = repository
                                           }

  (add, del, _) <- unzip3 . fmap (parseLine fileExtension) . T.lines <$> T.hGetContents hout

  let totalAdd = sum add
      totalDel = sum del

  void $ waitForProcess ph

  if verbose
    then
      T.putStrLn
      $  " ["
      <> author com
      <> "] "
      <> tshow (parentHashes com)
      <> " -> "
      <> commitHash com
      <> " | insertions:"
      <> tshow totalAdd
      <> " deletions: "
      <> tshow totalDel
      <> if (totalAdd + totalDel) == 0 then " (SKIPPED)" else ""
    else putChar (mkIcon (totalAdd + totalDel) (author com)) >> hFlush stdout

  return CommitStat { commit = com, insertions = sum add, deletions = sum del }


gitCommitSingleStat :: Options -> CommitInfo -> IO CommitStat
gitCommitSingleStat Options {..} com = do
  (_, Just hout, _, _) <- createProcess (proc "git" (["log", "--oneline", "--numstat"] <> [(T.unpack . commitHash) com])
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
      $  " ["
      <> author com
      <> "] "
      <> commitHash com
      <> " | insertions:"
      <> tshow totalAdd
      <> " deletions: "
      <> tshow totalDel
      <> if (totalAdd + totalDel) == 0 then " (SKIPPED)" else ""
    else putChar (mkIcon (totalAdd + totalDel) (author com)) >> hFlush stdout

  return CommitStat { commit = com, insertions = sum add, deletions = sum del }


sumStat :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sumStat (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)


aggregateStat :: [CommitStat] -> M.Map T.Text (Int, Int, Int)
aggregateStat ss =
  foldr (\st m -> M.insertWith sumStat ((author . commit) st) (insertions st, deletions st, 1) m) M.empty ss


totalStats :: M.Map T.Text (Int, Int, Int) -> (Int, Int, Int)
totalStats = foldr sumStat (0, 0, 0)


printStat :: M.Map T.Text (Int, Int, Int) -> IO ()
printStat m = void $ M.traverseWithKey
  (\k v ->
    T.putStrLn
      $  "   author: "
      <> k
      <> " -> "
      <> "insertions:"
      <> tshow (sel1 v)
      <> ", deletions: "
      <> tshow (sel2 v)
      <> ", commits: "
      <> tshow (sel3 v)
  )
  m

tshow :: Show a => a -> T.Text
tshow = T.pack . show
