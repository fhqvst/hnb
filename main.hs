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

probWord :: String -> M.Map String Double -> M.Map String Double -> Double
probWord string corpus vocab = (M.findWithDefault 1.0 string corpus) / fromIntegral (length $ (M.keys corpus) ++ (M.keys vocab))

-- P(C|D) = P(C) * P(w1|C) * P (w2|C) * ... * P(wn|C)
classify :: String -> M.Map String Double -> M.Map String Double -> Double -> Double
classify string corpus vocab prior = exp $ prior + sum probWords
  where probWords = map (\word -> log $ probWord word corpus vocab) (tokenize string)

main :: IO ()
main = do
 
  corpusPos <- parseData "data/positive.txt"
  corpusNeg <- parseData "data/negative.txt"

  let tokenizedPos = [ (tokenize string, Pos) | string <- lines corpusPos ]
  let tokenizedNeg = [ (tokenize string, Neg) | string <- lines corpusNeg ]

  -- Count words
  let posWords = concatMap fst tokenizedPos
  let negWords = concatMap fst tokenizedNeg
  let posCount = M.fromListWith (+) [ (word, 1.0) | word <- posWords ]
  let negCount = M.fromListWith (+) [ (word, 1.0) | word <- negWords ]
  let totCount = M.fromListWith (+) [ (word, 1.0) | word <- (posWords ++ negWords) ]
  
  -- Split into training vs. testing
  let splitPos = splitAt (floor $ 0.75 * fromIntegral (length tokenizedPos)) tokenizedPos
  let splitNeg = splitAt (floor $ 0.75 * fromIntegral (length tokenizedNeg)) tokenizedNeg

  let trainPos = fst splitPos
  let trainNeg = fst splitNeg

  let testPos = snd splitPos
  let testNeg = snd splitNeg

  -- Calculate prior probabilities
  let totalCount = sum $ M.elems totCount
  let priorPos = log $ sum (M.elems posCount) / totalCount
  let priorNeg = log $ sum (M.elems negCount) / totalCount

-- Classify a test string
  putStr "Input: "
  testString <- getLine 

  -- putStrLn $ printf "Positive: %.25f" $ classify testString posCount totCount priorPos
  -- putStrLn $ printf "Negative: %.25f" $ classify testString negCount totCount priorNeg

  let testPos = classify testString posCount totCount priorPos
  let testNeg = classify testString negCount totCount priorNeg

  sentiment <- do
    if (max testPos testNeg) == testPos
      then return $ "Positive: " ++ show testPos
      else return $ "Negative: " ++ show testNeg

  putStrLn sentiment
