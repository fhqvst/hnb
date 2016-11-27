module Main where

import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Text.Printf
import qualified Data.Map.Strict as M 
import Control.DeepSeq

data Sentiment = Pos | Neg
  deriving Show

tokenize :: String -> [String]
tokenize string = filter (not . null) $ splitOn " " [ toLower char | char <- string, char `notElem` ",.?!-:;\"\'" ]

parseData :: FilePath -> IO String
parseData fileName = withFile fileName ReadMode $
  \h -> do hSetEncoding h utf8_bom
           contents <- hGetContents h
           contents `deepseq` hClose h
           return contents

probWord :: String -> M.Map String Double -> Double
probWord string corpus = log $ (M.findWithDefault 1.0 string corpus) / fromIntegral (length (M.keys corpus))

classify :: String -> M.Map String Double -> Double -> Double
classify string corpus prior = exp $ prior + sum probWords
  where probWords = map (\word -> probWord word corpus) (tokenize string)

main :: IO ()
main = do
 
  corpusPos <- parseData "data/positive.txt"
  corpusNeg <- parseData "data/negative.txt"

  let tokenizedPos = [ (tokenize string, Pos) | string <- lines corpusPos ]
  let tokenizedNeg = [ (tokenize string, Pos) | string <- lines corpusNeg ]

  -- Count words
  let posWords = concatMap fst tokenizedPos
  let negWords = concatMap fst tokenizedNeg
  let posCount = M.fromListWith (+) [ (word, 1.0) | word <- posWords ]
  let negCount = M.fromListWith (+) [ (word, 1.0) | word <- negWords ]
  
  -- Split into training vs. testing
  let splitPos = splitAt (floor $ 0.75 * fromIntegral (length tokenizedPos)) tokenizedPos
  let splitNeg = splitAt (floor $ 0.75 * fromIntegral (length tokenizedNeg)) tokenizedNeg

  let trainPos = fst splitPos
  let trainNeg = fst splitNeg

  let testPos = snd splitPos
  let testNeg = snd splitNeg

  -- Calculate prior probabilities
  let totalCount = fromIntegral $ length (posWords ++ negWords)
  let priorPos = log $ sum (M.elems posCount) / totalCount
  let priorNeg = log $ sum (M.elems negCount) / totalCount

  -- Classify a test string
  let testString = "This movie was absolutely awful. I hated it. The movie sucks."  

  putStrLn $ printf "Positive: %.25f" $ classify testString posCount priorPos
  putStrLn $ printf "Negative: %.25f" $ classify testString negCount priorNeg
