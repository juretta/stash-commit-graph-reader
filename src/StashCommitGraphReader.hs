{-# LANGUAGE DoAndIfThenElse #-}

module StashCommitGraphReader (
    parse
  , Index(..)
  , IndexEntry(..)
) where

import qualified Data.ByteString.Lazy as BL
import           Text.Printf
import           Data.Binary.Get
import           Control.Monad
import           Data.List.Split
import           Data.List
import           Data.Word
import           Data.Bits

data Index = Index [IndexEntry] deriving (Eq)

newtype CommitId = CommitId [Word8] deriving (Eq)

instance Show Index where
    show (Index entries) = intercalate "\n" $ map show entries

instance Show CommitId where
    show (CommitId cid) = toHex cid

instance Show IndexEntry where
    show (IndexEntry cid parents') = show cid ++ " " ++ unwords (map show parents')

data IndexEntry = IndexEntry {
    _commitId :: !CommitId
  , _parents  :: [CommitId]
} deriving (Eq)

parse :: BL.ByteString -> Index
parse = runGet parseIndexFile

parseIndexFile :: Get Index
parseIndexFile = do
    indexEntries <- parseNext []
    return $ Index indexEntries
  where parseNext acc = do
            stop <- isEmpty
            return acc
            if stop then return acc
            else do
                iE <- parseIndexEntry
                parseNext (iE:acc)

parseIndexEntry :: Get IndexEntry
parseIndexEntry = do
    cId <- replicateM 20 getWord8
    l <- varlen
    parents' <- replicateM (numParents l) getWord8
    return (IndexEntry (CommitId cId) (fmap CommitId (chunksOf 20 parents')))
  where numParents word8 = fromIntegral word8 * 20


toHex :: (Monad m, PrintfType (m b), PrintfArg a) => m a -> m b
toHex bytes = bytes >>= printf "%02x"


varlen :: Get Int
varlen = varlen' 0 0
  where varlen' shft acc = do
            nextByte <- getWord8
            let bs = acc + ((coerce (nextByte .&. 0x7f) :: Int) `shiftL` shft)
            if isMsbSet nextByte
                then varlen' (shft+7) (bs+1)
                else return bs
        coerce = toEnum . fromEnum
        -- | Check whether the most significant bit of an octet is set.
        isMsbSet :: (Bits a, Num a) => a -> Bool
        isMsbSet x = (x .&. 0x80) /= 0

