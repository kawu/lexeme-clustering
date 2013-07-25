module NLP.LexemeClustering.NGrams
( ngrams
, ngram
, toFreq
) where


-- import           Control.Applicative (second)
import           Control.Arrow (second)
import           Data.Monoid (mconcat)
import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Text as T


-- | Collect n-grams of the given size throughout the collection
-- of words.  The resulting map will contain numbers of occurences
-- of the individual n-grams.
ngrams :: Int -> [T.Text] -> M.Map T.Text Int
ngrams k =
    let mkItem x = M.singleton x 1
    in  mconcat . map mkItem . mapMaybe (ngram k)


-- | Take n-gram of the given word.
ngram :: Int -> T.Text -> Maybe T.Text
ngram k x
    | n > k     = Just $ T.drop (n-k) x 
    | otherwise = Nothing
    where n = T.length x


-- | Convert a map with occurence numbers to a map
-- with frequencies.
toFreq :: Ord a => M.Map a Int -> M.Map a Double
toFreq m =
    let n = fromIntegral $ sum $ M.elems m
    in  M.fromList $ map (second $ (/n).fromIntegral) $ M.toList m
