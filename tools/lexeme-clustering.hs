{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           System.Console.CmdArgs
import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.DAWG.Static as D

import qualified NLP.LexemeClustering as LC


---------------------------------------
-- Command line options
---------------------------------------


-- | Clustering options.
data Cluster = Cluster
    { inputPath :: FilePath
    , freqMin   :: Double
    , nMax      :: Int
    , eps       :: Bool
    , kappa     :: Double }
    deriving (Data, Typeable, Show)


cluster :: Cluster
cluster = Cluster
    { inputPath = def &= argPos 0 &= typ "INPUT-FILE"
    , freqMin   = 0.001 &= help "N-gram frequency threshold"
    , nMax      = 8 &= help "Maximum n-gram length taken on account"
    , eps       = False &= help "Add epsilon to suffix set"
    , kappa     = 0.5 &= help "Kappa parameter" }
    &= summary "Grouping morphologically related words"
    &= program "lexeme-clustering"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgs cluster


exec :: Cluster -> IO ()
exec Cluster{..} = do
    putStrLn "# Collecting words"
    langDAWG <- D.weigh . D.fromLang . map T.unpack <$> LC.readWords inputPath
    putStr "# Number of language DAWG states: " >> print (D.numStates langDAWG)

    putStrLn "# Collecting suffixes"
    let ngCfg = LC.NGramConf
            { LC.freqMin = freqMin
            , LC.nMax    = nMax
            , LC.eps     = eps }
        sufDAWG = D.weigh $ D.fromLang
                $ map (T.unpack . fst)
                $ LC.ngrams ngCfg langDAWG
    putStr "# Number of suffix DAWG states: " >> print (D.numStates sufDAWG)

    putStrLn "# Suffix partiotioning"
    let sufDist = LC.mkSufDist langDAWG sufDAWG
    parMap <- LC.runCM sufDist kappa $ LC.partitionMap sufDAWG

    putStrLn "# Clustering"
    forM_ (LC.cluster langDAWG sufDAWG parMap) $ \xs -> do
        putStrLn $ intercalate ", " xs
