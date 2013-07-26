module NLP.LexemeClustering.Utils
( index
, byIndex
) where


import qualified Data.DAWG.Static as D


----------------------------------------
-- DAWG index
----------------------------------------


index :: Enum a => [a] -> D.DAWG a D.Weight c -> Int
index x dawg = case D.index x dawg of
    Nothing -> error "index: Nothing"
    Just i  -> i


byIndex :: Enum a => Int -> D.DAWG a D.Weight c -> [a]
byIndex i dawg = case D.byIndex i dawg of
    Nothing -> error "byIndex: Nothing"
    Just x  -> x
