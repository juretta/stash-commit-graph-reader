{-# LANGUAGE BangPatterns #-}

module Main where

import           Text.Printf
import           Data.Binary.Get
import           Control.Monad        
import qualified Data.ByteString.Lazy as BL
import           Data.List.Split
import           Data.List
import           Data.Word
import           System.Environment

data Index = Index [IndexEntry] deriving (Eq)

newtype CommitId = CommitId [Word8] deriving (Eq)

instance Show Index where
    show (Index entries) = intercalate "\n" $ map show entries

instance Show CommitId where
    show (CommitId cid) = toHex cid

instance Show IndexEntry where
    show (IndexEntry cid parents') = show cid ++ " " ++ (intercalate " " $ map show parents')

data IndexEntry = IndexEntry {
    commitId :: !CommitId
  , parents :: [CommitId]
} deriving (Eq)

main :: IO ()
main = do
    (file:_) <- getArgs
    content <- BL.readFile file
    let indexFile = runGet parseIndexFile content
    print indexFile

parseIndexFile :: Get Index
parseIndexFile = do
    indexEntries <- parseNext []
    return $ Index indexEntries
  where parseNext acc = do
            stop <- isEmpty
            if stop then 
                return acc
            else 
                do
            iE <- parseIndexEntry
            parseNext (iE:acc)

parseIndexEntry :: Get IndexEntry
parseIndexEntry = do
    cId <- replicateM 20 getWord8
    l <- getWord8 -- FIXME: Respect var length encoding although in practice this is unlikely to be an issue
    parents' <- replicateM (numParents l) getWord8
    return (IndexEntry (CommitId cId) (fmap CommitId (chunksOf 20 parents')))
  where numParents word8 = (fromIntegral word8) * 20


toHex :: (Monad m, PrintfType (m b), PrintfArg a) => m a -> m b
toHex bytes = bytes >>= printf "%02x"

