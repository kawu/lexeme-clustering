{-# LANGUAGE DeriveFunctor #-}


module NLP.LexemeClustering.DAWG
( intersection
, intersectionWith
) where


import           Data.Vector.Unboxed (Unbox)
import qualified Data.DAWG.Static as D


-- | A trie with characters of type @a@ and values of type @b@.
data Trie a b = Trie
    { value :: Maybe b
    , edges :: [(a, Trie a b)] }
    deriving (Show, Eq, Ord, Functor)


-- | An intersection between two 'DAWG's.
-- Assumption: the 'D.edges' function returns elements in a
-- strictly ascending order with respect to the symbol elements.
intersectionWith
    :: (Enum a, Ord a, Unbox b)
    => (c -> d -> e)
    -> D.DAWG a b c
    -> D.DAWG a b d
    -> Trie a e
intersectionWith f dawg dawg' = Trie
    { value = do
        x <- D.lookup [] dawg
        y <- D.lookup [] dawg'
        return $ f x y
    , edges = merge (D.edges dawg) (D.edges dawg') }
  where
    merge xs@((x, d1):xs') ys@((y, d2):ys')
        | x < y     = merge xs  ys'
        | x > y     = merge xs' ys
        | otherwise = (x, intersectionWith f d1 d2)
                    : merge xs' ys'
    merge _ _   = []


-- | An intersection between two 'DAWG's.
intersection
    :: (Enum a, Ord a, Unbox b)
    => D.DAWG a b c
    -> D.DAWG a b d
    -> Trie a c
intersection = intersectionWith const
