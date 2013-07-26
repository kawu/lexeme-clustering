{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}


module NLP.LexemeClustering.DAWG
( 
-- * Trie
  Trie (..)
, unTrie
  
-- * DAWG
-- ** Intersection
, intersection
, intersectionWith
-- ** Sub-DAWGs
, subDAWGs
) where


import           Data.Vector.Unboxed (Unbox)
import qualified Data.DAWG.Static as D


--------------------------------------------
-- Trie
--------------------------------------------


-- | A trie with characters of type @a@ and values of type @b@.
data Trie a b = Trie
    { value :: Maybe b
    , edges :: [(a, Trie a b)] }
    deriving (Show, Eq, Ord, Functor)


-- | Make set from a 'Trie'.
unTrie :: Trie a b -> [([a], b)]
unTrie Trie{..} = case value of
    Nothing -> subTries
    Just x  -> ([], x) : subTries
  where
    subTries = concat
        [ map (addSym x) (unTrie subTrie)
        | (x, subTrie) <- edges ]
    addSym x (xs, t) = (x:xs, t)


--------------------------------------------
-- Intersection
--------------------------------------------


-- | An intersection between two 'DAWG's.
-- Assumption: the 'D.edges' function returns elements in a
-- strictly ascending order with respect to the symbol elements.
intersectionWith
    :: ( Enum a, Ord a
       , Unbox b, Unbox c )
    => (d -> e -> f)
    -> D.DAWG a b d
    -> D.DAWG a c e
    -> Trie a f
intersectionWith f dawg dawg' = Trie
    { value = do
        x <- D.lookup [] dawg
        y <- D.lookup [] dawg'
        return $ f x y
    , edges = merge (D.edges dawg) (D.edges dawg') }
  where
    merge xs@((x, d1):xs') ys@((y, d2):ys')
        | x < y     = merge xs' ys
        | x > y     = merge xs  ys'
        | otherwise = (x, intersectionWith f d1 d2)
                    : merge xs' ys'
    merge _ _   = []


-- | An intersection between two 'DAWG's.
intersection
    :: ( Enum a, Ord a
       , Unbox b, Unbox c )
    => D.DAWG a b d
    -> D.DAWG a c e
    -> Trie a d
intersection = intersectionWith const


--------------------------------------------
-- Sub-DAWGs
--------------------------------------------


-- | List all sub'D.DAWG's (including the given 'DAWG') of the given 'D.DAWG'.
-- Note: sub-DAWGs which occur multiple times in the given 'DAWG' (i.e. are
-- reachable through multiple paths from the root node) will be also present
-- multiple times in the resulting list.
subDAWGs :: Enum a => D.DAWG a b c -> [D.DAWG a b c]
subDAWGs dawg = dawg : concatMap (subDAWGs.snd) (D.edges dawg)
