{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


module NLP.LexemeClustering
(
-- * N-grams
  NGramConf (..)
, ngrams
-- * I/O
, readWords
-- * Suffix set
, SufSet
, decode
-- * Suffix distribution
, mkSufDist
, printSufDist
-- * Entropy and mutual information
, CMEnv (..)
, CMState
, CM
, runCM
, entropy
, mutual
-- * Suffix set partitioning
, partition
, partitionMap
-- * Lexeme clustering
, cluster
) where


import           Control.Monad (forM_, guard, foldM)
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
import           Control.Monad.State.Strict (lift, liftIO, MonadIO)
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Reader as R
import qualified Data.DAWG.Static as D

import qualified NLP.LexemeClustering.NGrams as NG
import qualified NLP.LexemeClustering.Dist as P
import qualified NLP.LexemeClustering.DisjointSet as J
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
    , nMax      :: Int
    -- | Add epsilon suffix.
    , eps       :: Bool }
    deriving (Show, Eq, Ord)


-- | Compute a list of suffix n-grams with lengths of the @[1..n]@ form,
-- where @n@ is the given argument.  Only ngrams with sufficiently
-- high frequency will be preserved.
ngrams
    :: Unbox b
    => NGramConf
    -> D.DAWG Char b c      -- ^ Language automaton
    -> [(T.Text, Double)]
ngrams NGramConf{..} dawg =
    withEps $ concatMap ngramsFor [1 .. nMax]
  where
    withEps | eps       = (("", 1) :)
            | otherwise = id
    ngramsFor n =
        let ngs = P.toDist $ NG.ngrams n $ map T.pack $ D.keys dawg
        in  reverse $ sortBy (comparing snd)
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
    [ M.singleton (mkSufSet sufDAWG subDAWG) 1
    | subDAWG <- map snd $ traverse langDAWG ]


-- | Make suffix set from two automatons.
mkSufSet
    :: (Enum a, Ord a, Unbox b)
    => D.DAWG a D.Weight c      -- ^ Suffix automaton
    -> D.DAWG a b c             -- ^ Language sub-automaton
    -> SufSet                   -- ^ Suffix set
mkSufSet sufDAWG subDAWG
    = I.fromList
    $ map (flip index sufDAWG)
    $ map fst $ unTrie
    $ intersection sufDAWG subDAWG


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


-- | Environment of the clustering monad.
data CMEnv = CMEnv {
    -- | Base distribution.
      baseDist  :: P.Dist SufSet
    -- | Normalize mutual information? 
    , normMut   :: Bool
    -- | Kappa parameter (maximal mutual information between disjoint subsets).
    , kappa     :: Double }


-- | State of the clustering monad: memoization map for entropy values.
type CMState = M.Map SufSet Double


-- | A clustering monad.
type CM m a = ST.StateT CMState (R.ReaderT CMEnv m) a


-- | Run the CM monad w.r.t. the base distribution and the Kappa parameter.
runCM :: Monad m => CMEnv -> CM m a -> m a
runCM env cm = R.runReaderT (ST.evalStateT cm M.empty) env
-- runCM env cm = ST.evalStateT (R.runReaderT cm env) M.empty


-- | Compute entropy of the given suffix set.
-- Results are memoized.
entropy :: Monad m => SufSet -> CM m Double
entropy x = do
    CMEnv{..} <- R.ask
    entrMemo  <- ST.get
    case M.lookup x entrMemo of
        Just e  -> return e
        Nothing -> do
            let e = P.entropy $ varDist x baseDist
            ST.put $ M.insert x e entrMemo
            return e


-- | Normalized mutual information between two suffix sets.
mutual :: Monad m => SufSet -> SufSet -> CM m Double
mutual x y = do
    doNorm <- R.asks normMut
    ex <- entropy x
    ey <- entropy y
    e2 <- entropy $ I.union x y
    let mi = ex + ey - e2
    return $ if doNorm
        then mi / (min ex ey)
        else mi


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
partition :: (Functor m, Monad m) => SufSet -> CM m (S.Set SufSet)
partition sufSet =
    iterWhile updatePar par0
  where
    par0 = S.fromList
        [ I.singleton x
        | x <- I.toList sufSet ]
    updatePar xs = runMaybeT $ do
        (x, y, mi) <- max3 =<< sequence
            [ (x, y, ) <$> lift (mutual x y)
            | (x, y) <- pairs (S.toList xs) ]
        k <- R.asks kappa
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


-- | Compute suffix partitions for individual suffix sets
-- on the basis of the suffix distribution.
partitionMap
    :: (Functor m, Monad m, MonadIO m)
    => D.DAWG Char D.Weight c
    -> CM m (M.Map SufSet (S.Set SufSet))
partitionMap sufDAWG = do
    sufDist <- R.asks baseDist
    let foldM' x xs f = foldM f x xs
    foldM' M.empty (M.keys sufDist) $ \sufMap sufSet -> do
        let showSs xs = "{" ++ intercalate ", " xs ++ "}"
        sufPar <- partition sufSet
        liftIO $ do
            putStr $ showSs $ decode sufDAWG sufSet
            putStr " => "
            putStrLn $ intercalate "; " $
                map (showSs . decode sufDAWG) (S.toList sufPar)
        return $ M.insert sufSet sufPar sufMap


----------------------------------------
-- Lexeme clustering
----------------------------------------


-- | Perform clustering w.r.t. the partition map.
cluster
    :: (Enum a, Ord a)
    => D.DAWG a D.Weight c          -- ^ Language automaton
    -> D.DAWG a D.Weight c          -- ^ Suffix automaton
    -> M.Map SufSet (S.Set SufSet)  -- ^ Partition map
    -> [[[a]]]
cluster langDAWG sufDAWG parMap =
    [ map (flip byIndex langDAWG) clt
    | clt <- J.toList disj ]
  where
    -- Disjoint set.
    disj = J.fromList (D.size langDAWG) eqRel
    -- Equivalence relation pairs.
    eqRel = concatMap eqRelOn $ traverse langDAWG
    -- Equivalence relation pairs at the given position in the language DAWG.
    eqRelOn (stem, subDAWG) = concat
        [ pairs
            [ index (stem ++ suf) langDAWG
            | suf <- decode sufDAWG part ]
        | part <- sufPar ]
      where
        sufSet = mkSufSet sufDAWG subDAWG
        sufPar = S.toList $ parMap M.! sufSet
