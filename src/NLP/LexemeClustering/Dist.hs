module NLP.LexemeClustering.Dist
( Dist
, toDist
, entropy
) where


import           Control.Arrow (second)
import qualified Data.Map.Strict as M


-- | A random distribution over elements of type @a@.
type Dist a = M.Map a Double


-- | Convert a map with occurence numbers to a map
-- with frequencies (empirical distribution).
toDist :: Ord a => M.Map a Int -> Dist a
toDist m =
    let n = fromIntegral $ sum $ M.elems m
        norm = second $ (/n) . fromIntegral
    in  M.fromList $ map norm $ M.toList m


-- | Entropy of the distribution.
-- Assumption: there are no 0-probability elements in the distribution.
entropy :: Ord a => Dist a -> Double
entropy dist = negate $ sum
    [ p * logBase 2 p
    | (_, p) <- M.toList dist ]
