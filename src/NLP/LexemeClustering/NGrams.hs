-- | Collecting suffix n-grams.


module NLP.LexemeClustering.NGrams
( ngrams
, ngram
) where


import           Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- | Collect suffix n-grams of the given size throughout the collection
-- of words.  The resulting map will contain numbers of occurences
-- of the individual n-grams.
ngrams :: Int -> [T.Text] -> M.Map T.Text Int
ngrams k =
    let mkItem x = M.singleton x 1
    in  M.unionsWith (+) . map mkItem . mapMaybe (ngram k)


-- | Take suffix n-gram of the given word.
ngram :: Int -> T.Text -> Maybe T.Text
ngram k x
    | n > k     = Just $ T.drop (n-k) x 
    | otherwise = Nothing
    where n = T.length x
