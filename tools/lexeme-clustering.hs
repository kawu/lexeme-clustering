{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           System.Console.CmdArgs
import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           Control.Monad.Trans.Class (lift)
import           Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map as M
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
    , kappa     :: Double }
    deriving (Data, Typeable, Show)


cluster :: Cluster
cluster = Cluster
    { inputPath = def &= argPos 0 &= typ "INPUT-FILE"
    , freqMin   = 0.001 &= help "Ngram grequency threshold"
    , nMax      = 8 &= help "Maximum ngram length taken on account"
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
    putStrLn "Collecting suffixes"
    let ngCfg = LC.NGramConf { LC.freqMin = freqMin, LC.nMax = nMax }
    sufDAWG <- D.weigh . D.fromLang . map (T.unpack . fst)
        <$> LC.readNGrams ngCfg inputPath
    putStr "Number of suffix DAWG states: " >> print (D.numStates sufDAWG)

    putStrLn "Collecting words"
    langDAWG <- D.fromLang . map T.unpack <$> LC.readWords inputPath
    putStr "Number of language DAWG states: " >> print (D.numStates langDAWG)

    putStrLn "Compute suffix distribution"
    let sufDist = LC.mkSufDist langDAWG sufDAWG
    -- LC.printSufDist sufDAWG sufDist
    LC.runCM sufDist kappa $ do
        forM_ (M.keys sufDist) $ \sufSet -> do
            let showSs xs = "{" ++ intercalate ", " xs ++ "}"
            sufPar <- S.toList <$> LC.partition sufSet sufDAWG
            lift $ do
                putStr $ showSs $ LC.decode sufDAWG sufSet
                putStr " => "
                putStrLn $ intercalate "; " $
                    map (showSs . LC.decode sufDAWG) sufPar
