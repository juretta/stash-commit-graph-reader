{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           System.Console.CmdArgs
import           System.Environment      (getArgs, withArgs)
import           StashCommitGraphReader
import           Control.Arrow
import           Text.Printf
import qualified Data.ByteString.Lazy as BL

data CommitGraphReader = CommitGraphReader {
    file :: FilePath
  , stats :: Bool
} deriving (Data,Typeable,Show)

mode = cmdArgsMode $ CommitGraphReader {
                file = "" &= argPos 0 &= typ "FilePath"
              , stats = False &= typ "BOOL"
            }
    &= help "Retrieve information about the commit graph cache files"
    &= program "stash-commit-graph-reader"

main = do
    options <- getArgs
    config <- (if null options then withArgs ["--help"] else id) $ cmdArgsRun mode
    run config


readCacheFile :: FilePath -> (BL.ByteString -> a) -> IO a
readCacheFile f handler = do
    content <- BL.readFile f
    return $ handler content

run :: CommitGraphReader -> IO ()
run (CommitGraphReader f True)  = printCacheFileStats f
run (CommitGraphReader f False) = printCacheFile f

printCacheFile :: FilePath -> IO ()
printCacheFile f = do
    indexFile <- readCacheFile f parse
    print indexFile

printCacheFileStats :: FilePath -> IO ()
printCacheFileStats f = do
    (bytes, indexFile) <- readCacheFile f (BL.length &&& parse)
    let text = show indexFile
        bytesInflated = length text
    let (commits, commitsIncludingParentCommits) = countCommits indexFile
    printf "Number of commits: %d\n" commits
    printf "Number of commits including parent commits: %d\n" commitsIncludingParentCommits
    printf "Bytes (binary): %d\n" bytes
    printf "Bytes (text): %d\n" bytesInflated
    printf "Compression ratio: %2.2f%%\n" (((fromIntegral bytes/fromIntegral bytesInflated) * 100) :: Double)
    where countCommits (Index entries) = let countCommitsPerEntry (IndexEntry _ parents) = 1 + length parents
                                         in (length entries, sum $ map countCommitsPerEntry entries)
