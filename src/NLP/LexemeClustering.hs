{-# LANGUAGE RecordWildCards #-}


module NLP.LexemeClustering
(
-- * N-grams
  NGramConf (..)
, readNGrams
-- * I/O
, readWords
-- * Suffix distribution
, mkSufDist
, printSufDist
-- * Entropy and mutual information
, CMState (..)
, CM
, runCM
, entropy
, mutual
) where


import           Control.Monad (forM, forM_)
import           Control.Applicative ((<$>))

import           Data.Ord (comparing)
import           Data.List (sortBy, intercalate)
import qualified Data.IntSet as I
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Vector.Unboxed (Unbox)
import qualified Control.Monad.State.Strict as ST
import qualified Data.DAWG.Static as D

import qualified NLP.LexemeClustering.NGrams as NG
import qualified NLP.LexemeClustering.Dist as P
import           NLP.LexemeClustering.Utils
import           NLP.LexemeClustering.DAWG


----------------------------------------
-- N-grams
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
        ngs <- P.toDist . NG.ngrams n <$> readWords path
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
-- Suffix distribution
----------------------------------------


-- | A suffix set encoded w.r.t. the suffix automaton.
type SufSet = I.IntSet


-- | Compute numbers of individual suffix sets in the automaton.
-- Individual IDs in the output maps correspond to IDs in the
-- suffix automaton.
mkSufDist
    :: (Enum a, Ord a, Unbox b)
    => D.DAWG a b c             -- ^ Language automaton
    -> D.DAWG a (D.Weight) c    -- ^ Suffix automaton
    -> P.Dist SufSet            -- ^ Suffix distribution
mkSufDist langDAWG sufDAWG = P.toDist $ M.unionsWith (+)
    [ let sufIDs
            = I.fromList
            $ map (flip index sufDAWG)
            $ map fst $ unTrie
            $ intersection sufDAWG subDAWG
      in  M.singleton sufIDs 1
    | subDAWG <- subDAWGs langDAWG ]


-- | Print information about numbers of individual suffix sets.
printSufDist
    :: D.DAWG Char (D.Weight) c -- ^ Suffix automaton
    -> P.Dist SufSet            -- ^ Suffix distribution
    -> IO ()
printSufDist sufDAWG sufDist = do
    forM_ (M.toList sufDist) $ \(sufSet, sufFreq) -> do
        let sufSet' = map (flip byIndex sufDAWG) (I.toList sufSet)
        putStr $ "{" ++ intercalate ", " sufSet' ++ "}: "
        print sufFreq


-----------------------------------------------------------------------
-- Random variable distribution
--
-- Here, a random variable is also a suffix set.  Value of the random
-- variable w.r.t. the elementary suffix set is the intersection
-- of the two sets.
-----------------------------------------------------------------------


-- | Distribution of the random variable w.r.t. the base distribution.
varDist :: SufSet -> P.Dist SufSet -> P.Dist SufSet
varDist sufSet sufDist = M.fromListWith (+)
    [ (I.intersection sufSet sufSet', q)
    | (sufSet', q) <- M.toList sufDist ]


----------------------------------------
-- Entropy and mutual information
----------------------------------------


-- | State of the clustering monad serves to keep already computed
-- entropy values.
data CMState = CMState
    { entrMemo  :: M.Map SufSet Double
    , baseDist  :: P.Dist SufSet }


-- | A clustering monad.
type CM a = ST.State CMState a


-- | Run the CM monad w.r.t. the base distribution.
runCM :: P.Dist SufSet -> CM a -> a
runCM dist = flip ST.evalState $ CMState
    { entrMemo = M.empty
    , baseDist = dist }


-- | Compute entropy of the given suffix set.
-- Results are memoized.
entropy :: SufSet -> CM Double
entropy x = do
    st@CMState{..} <- ST.get
    case M.lookup x entrMemo of
        Just e  -> return e
        Nothing -> do
            let e = P.entropy $ varDist x baseDist
                memo = M.insert x e entrMemo
            ST.put $ st { entrMemo = memo }
            return e


-- | Mutual information between two suffix sets.
mutual :: SufSet -> SufSet -> CM Double
mutual x y = do
    ex <- entropy x
    ey <- entropy y
    e2 <- entropy $ I.union x y
    return $ ex + ey - e2
