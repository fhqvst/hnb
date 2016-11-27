module Main where

import Data.List
import Data.List.Split
import Data.Char
import Text.Printf
import qualified Data.Map.Strict as M

data Sentiment = Pos | Neg
  deriving Show

tokenize :: String -> [String]
tokenize string = filter (not . null) $ splitOn " " [ toLower char | char <- string, char `notElem` ",.?!-:;\"\'" ]

positive = [("This is some awesome text", Pos),("The text is really good", Pos),("The food was great!", Pos)]

negative = [("What an awful text this is", Neg),("I absolutely hate these words", Neg),("The food was bad! I hated every second", Neg)]
-- Tokenize sentences
tokenizedPos = map (\(t,s) -> (tokenize t, s)) positive 
tokenizedNeg = map (\(t,s) -> (tokenize t, s)) negative

-- Count words
posWords = concatMap fst tokenizedPos
negWords = concatMap fst tokenizedNeg 
posCount = M.fromListWith (+) [ (word, 1.0) | word <- posWords ]
negCount = M.fromListWith (+) [ (word, 1.0) | word <- negWords ]

-- Split into training vs. testing
splitPos = splitAt (floor $ 0.75 * fromIntegral (length tokenizedPos)) tokenizedPos
splitNeg = splitAt (floor $ 0.75 * fromIntegral (length tokenizedNeg)) tokenizedNeg

trainPos = fst splitPos
trainNeg = fst splitNeg

testPos = snd splitPos
testNeg = snd splitNeg

-- Calculate prior probabilities
totalCount = fromIntegral $ length (posWords ++ negWords)
priorPos = sum (M.elems posCount) / totalCount
priorNeg = sum (M.elems negCount) / totalCount

probWord :: String -> M.Map String Double -> Double
probWord string corpus = (M.findWithDefault 1.0 string corpus) / fromIntegral (length (M.keys corpus))

classifyPos :: String -> Double
classifyPos string = priorPos * product probWords
  where probWords = map (\word -> probWord word posCount) (tokenize string)

classifyNeg :: String -> Double
classifyNeg string = priorNeg * product probWords
  where probWords = map (\word -> probWord word negCount) (tokenize string)

main :: IO ()
main = do
  -- Classify a test string
  let testString = "Absolutely, I hate these... words."  

  putStrLn $ printf "Positive: %.9f" $ classifyPos testString
  putStrLn $ printf "Negative: %.9f" $ classifyNeg testString
  
  putStrLn "Finished" 
