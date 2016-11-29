import qualified NaiveBayes as NB
import System.IO
import Control.DeepSeq
import Control.Monad
import Text.Printf
import qualified Data.Map.Strict as M

parseData :: FilePath -> IO String
parseData fileName = withFile fileName ReadMode $
  \h -> do hSetEncoding h utf8_bom
           contents <- hGetContents h
           contents `deepseq` hClose h
           return contents

main = do

  -- Load phrases from files
  posPhrases <- parseData "data/positive.txt"
  negPhrases <- parseData "data/negative.txt"

  -- Build corpuses from phrases
  let corpusPos = [ (string, NB.pos) | string <- lines posPhrases ]
  let corpusNeg = [ (string, NB.neg) | string <- lines negPhrases ]
  
  -- Train a model using corpuses
  let corpus = corpusPos ++ corpusNeg

  putStrLn "\nTraining model..."
  let model = NB.train NB.empty corpus

  -- Test the model
  -- @todo
  
  putStrLn $ "- Vocabulary: " ++ show (length (M.elems (NB.vocabulary model)))
  putStrLn $ "- Positive: " ++ show (length (M.elems (NB.positive model)))
  putStrLn $ "- Negative: " ++ show (length (M.elems (NB.negative model)))
  putStrLn $ "- Positive documents: " ++ show (M.findWithDefault 0.0 NB.pos $ NB.counts model)
  putStrLn $ "- Negative documents: " ++ show (M.findWithDefault 0.0 NB.neg $ NB.counts model)

  putStrLn ""

  forever $ do
    putStr "Input: "
    string <- getLine
    printf $ "--- " ++ (show (NB.classify model string)) ++ "\n"
