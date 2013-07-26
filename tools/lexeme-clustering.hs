{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           System.Console.CmdArgs
import           Control.Applicative ((<$>))
-- import           Control.Monad (forM_)
-- import qualified Data.Map as M
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.DAWG.Static as D

import qualified NLP.LexemeClustering as LC
-- import           NLP.LexemeClustering.NGrams
-- import           NLP.LexemeClustering.DAWG


---------------------------------------
-- Command line options
---------------------------------------


-- | Clustering options.
data Cluster = Cluster
    { inputPath :: FilePath
    , freqMin   :: Double
    , nMax      :: Int }
    deriving (Data, Typeable, Show)


cluster :: Cluster
cluster = Cluster
    { inputPath = def &= argPos 0 &= typ "INPUT-FILE"
    , freqMin   = 0.001 &= help "Ngram grequency threshold"
    , nMax      = 8 &= help "Maximum ngram length taken on account" }
    &= summary "Grouping morphologically related words"
    &= program "lexeme-clustering"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgs cluster


exec :: Cluster -> IO ()
exec Cluster{..} = do
    putStrLn "Collecting suffixes"
    let ngCfg = LC.NGramConf { LC.freqMin = freqMin, LC.nMax = nMax }
    sufDAWG <- D.weigh . D.fromLang . map (T.unpack . fst)
        <$> LC.readNGrams ngCfg inputPath
    putStr "Number of suffix DAWG states: " >> print (D.numStates sufDAWG)

    putStrLn "Collecting words"
    langDAWG <- D.fromLang . map T.unpack <$> LC.readWords inputPath
    putStr "Number of language DAWG states: " >> print (D.numStates langDAWG)

    putStrLn "Compute map with suffix sets"
    let sufMap = LC.collSufs langDAWG sufDAWG
    LC.printSufs sufDAWG sufMap
