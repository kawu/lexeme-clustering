{-# LANGUAGE RecordWildCards #-}


module NLP.LexemeClustering
(
-- * N-grams
  NGramConf (..)
, readNGrams
-- * I/O
, readWords
-- * Suffix sets
, collSufs
, printSufs
) where


import           Control.Monad (forM, forM_)
import           Control.Applicative ((<$>))

import           Data.Ord (comparing)
import           Data.List (sortBy, intercalate)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Vector.Unboxed (Unbox)
import qualified Data.DAWG.Static as D

import qualified NLP.LexemeClustering.NGrams as NG
import           NLP.LexemeClustering.DAWG


----------------------------------------
-- n-grams
----------------------------------------


-- | Configuration for constructing n-grams.
data NGramConf = NGramConf {
    -- | Minimum frequency of an n-gram.
      freqMin   :: Double
    -- | Maximum length of n-grams taken into account.
    , nMax      :: Int }
    deriving (Show, Eq, Ord)


-- | Read a list of suffix n-grams with lengths of the @[1..n]@ form,
-- where @n@ is the given argument.  Only ngrams with sufficiently
-- high frequency will be preserved.
readNGrams :: NGramConf -> FilePath -> IO [(T.Text, Double)]
readNGrams NGramConf{..} path = concat <$> do
    forM [1..nMax] $ \n -> do
        ngs <- NG.toFreq . NG.ngrams n <$> readWords path
        return $ reverse $ sortBy (comparing snd)
            [x | x <- M.toList ngs, snd x >= freqMin]


----------------------------------------
-- I/O
----------------------------------------


-- | Read a list of words from the given file.
-- Every word has to be kept in a separate line.
readWords :: FilePath -> IO [T.Text]
readWords path = map L.toStrict . L.lines <$> L.readFile path


----------------------------------------
-- Suffix sets
----------------------------------------


-- | Compute numbers of individual suffix sets in the automaton.
-- Individual IDs in the output maps correspond to IDs in the
-- suffix automaton.
collSufs
    :: (Enum a, Ord a, Unbox b)
    => D.DAWG a b c             -- ^ Language automaton
    -> D.DAWG a (D.Weight) c    -- ^ Suffix automaton
    -> M.Map (S.Set Int) Int    -- ^ Numbers of suffix subsets
collSufs langDAWG sufDAWG = M.unionsWith (+)
    [ let sufIDs
            = S.fromList
            $ map (flip index sufDAWG)
            $ map fst $ unTrie
            $ intersection sufDAWG subDAWG
      in  M.singleton sufIDs 1
    | subDAWG <- subDAWGs langDAWG ]


-- | Print information about numbers of individual suffix sets.
printSufs
    :: D.DAWG Char (D.Weight) c -- ^ Suffix automaton
    -> M.Map (S.Set Int) Int    -- ^ Numbers of suffix subsets
    -> IO ()
printSufs sufDAWG sufMap = do
    forM_ (M.toList sufMap) $ \(sufSet, sufNum) -> do
        let sufSet' = map (flip byIndex sufDAWG) (S.toList sufSet)
        putStr $ "{" ++ intercalate ", " sufSet' ++ "}: "
        print sufNum 


----------------------------------------
-- Utils
----------------------------------------


index :: Enum a => [a] -> D.DAWG a D.Weight c -> Int
index x dawg = case D.index x dawg of
    Nothing -> error "index: Nothing"
    Just i  -> i


byIndex :: Enum a => Int -> D.DAWG a D.Weight c -> [a]
byIndex i dawg = case D.byIndex i dawg of
    Nothing -> error "byIndex: Nothing"
    Just x  -> x
