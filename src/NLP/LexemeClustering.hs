{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


module NLP.LexemeClustering
(
-- * N-grams
  NGramConf (..)
, readNGrams
-- * I/O
, readWords
-- * Suffix set
, SufSet
, decode
-- * Suffix distribution
, mkSufDist
, printSufDist
-- * Entropy and mutual information
, CMState (..)
, CM
, runCM
, entropy
, mutual
-- * Suffix set partitioning
, partition
) where


import           Control.Monad (forM, forM_, guard, when)
import           Control.Applicative ((<$>))

import           Data.Ord (comparing)
import           Data.List (sortBy, maximumBy, intercalate)
import qualified Data.IntSet as I
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Vector.Unboxed (Unbox)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.State.Strict (lift, MonadIO(..), liftIO)
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
-- Suffix set
----------------------------------------


-- | A suffix set encoded w.r.t. the suffix automaton.
type SufSet = I.IntSet


-- | Decode suffix set.
decode :: Enum a => D.DAWG a D.Weight c -> SufSet -> [[a]]
decode dawg = map (flip byIndex dawg) . I.toList


----------------------------------------
-- Suffix distribution
----------------------------------------


-- | Compute numbers of individual suffix sets in the automaton.
-- Individual IDs in the output maps correspond to IDs in the
-- suffix automaton.
mkSufDist
    :: (Enum a, Ord a, Unbox b)
    => D.DAWG a b c             -- ^ Language automaton
    -> D.DAWG a D.Weight c      -- ^ Suffix automaton
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
        let sufSet' = decode sufDAWG sufSet
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


-- | State of the clustering monad.
data CMState = CMState {
    -- | Memoization map for entropy values.
      entrMemo  :: M.Map SufSet Double
    -- | Base distribution.
    , baseDist  :: P.Dist SufSet
    -- | Kappa parameter (maximal mutual information of disjoint subsets).
    , kappa     :: Double }


-- | A clustering monad.
type CM m a = ST.StateT CMState m a


-- | Run the CM monad w.r.t. the base distribution and the Kappa parameter.
runCM :: Monad m => P.Dist SufSet -> Double -> CM m a -> m a
runCM dist kappa = flip ST.evalStateT $ CMState
    { entrMemo  = M.empty
    , baseDist  = dist
    , kappa     = kappa }


-- | Compute entropy of the given suffix set.
-- Results are memoized.
entropy :: Monad m => SufSet -> CM m Double
entropy x = do
    st@CMState{..} <- ST.get
    case M.lookup x entrMemo of
        Just e  -> return e
        Nothing -> do
            let e = P.entropy $ varDist x baseDist
                memo = M.insert x e entrMemo
            ST.put $ st { entrMemo = memo }
            return e


-- | Normalized mutual information between two suffix sets.
mutual :: (Monad m, MonadIO m) => D.DAWG Char D.Weight c -> SufSet -> SufSet -> CM m Double
mutual sufDAWG x y = do
    let showSs xs = "{" ++ intercalate ", " xs ++ "}"
    ex <- entropy x
    ey <- entropy y
    e2 <- entropy $ I.union x y
    let mi = ex + ey - e2
    return $ mi / (min ex ey)


-- -- | Mutual information between two suffix sets.
-- mutual :: (Monad m, MonadIO m) => D.DAWG Char D.Weight c -> SufSet -> SufSet -> CM m Double
-- mutual sufDAWG x y = do
--     let showSs xs = "{" ++ intercalate ", " xs ++ "}"
--     ex <- entropy x
--     ey <- entropy y
--     e2 <- entropy $ I.union x y
--     let mi = ex + ey - e2
--     -- when (e2 == ey) $ liftIO $ do
--     when (mi > min ex ey) $ liftIO $ do
--         putStr "Mutual: "
--         putStr $ showSs $ decode sufDAWG x
--         putStr ", "
--         putStr $ showSs $ decode sufDAWG y
--         putStr " => "
--         print (ex, ey, e2, mi)
--     return mi


----------------------------------------
-- Suffix set partitioning
----------------------------------------


-- | Partition the given suffix set into disjoint subsets.
partition :: (Functor m, Monad m, MonadIO m) => SufSet -> D.DAWG Char D.Weight c -> CM m (S.Set SufSet)
partition sufSet sufDAWG =
    iterWhile updatePar par0
  where
    par0 = S.fromList
        [ I.singleton x
        | x <- I.toList sufSet ]
    updatePar xs = runMaybeT $ do
        (x, y, mi) <- max3 =<< (lift.sequence)
            [ (x, y, ) <$> mutual sufDAWG x y
            | x <- S.toList xs
            , y <- S.toList xs, x /= y ]
        k <- ST.gets kappa
        guard $ mi >= k
        return $ S.insert (I.union x y)
               $ S.delete x
               $ S.delete y xs
    max3 [] = MaybeT $ return Nothing
    max3 xs = return $ maximumBy (comparing _3) xs
    _3 (_, _, x) = x
    

-- | Iterate over the value with the given monadic function. 
iterWhile :: Monad m => (a -> m (Maybe a)) -> a -> m a
iterWhile f x = do
    mx <- f x
    case mx of
        Nothing -> return x
        Just y  -> iterWhile f y
