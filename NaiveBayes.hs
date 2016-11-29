module NaiveBayes (
  Classifier,
  Sentiment,
  vocabulary,
  positive,
  negative,
  counts,
  empty,
  mapFst,
  tokenize,
  train,
  test,
  classify,
  pos,
  neg
) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Char as C

-- Sentiment constructors
data Sentiment = Pos | Neg 
  deriving (Eq, Ord, Show)
pos = Pos
neg = Neg

-- A Datapoint maps a string to a sentiment
type Datapoint = (String, Sentiment)

-- A CountVector describes the number of occurrences
-- for any string/word. Probably a bad name since it
-- isn't really a vector at all.
type CountVector = M.Map String Double

-- The Naive Bayes classifier
data Classifier = Classifier { 
  vocabulary :: CountVector,        -- All words, and their respective counts
  positive :: CountVector,          -- All positive words, and their respective counts
  negative :: CountVector,          -- All negative words, and their respective counts
  counts :: M.Map Sentiment Double  -- The number of positive and negative documents
}

-- Constructor for creating a new empty classifier
empty :: Classifier
empty = Classifier M.empty M.empty M.empty M.empty

-- Apply any function to the first value in a tuple when
-- mapping over lists of tuples
mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f t = map (\(x, y) -> (f x, y)) t

-- Tokenize strings into an array of tokens
tokenize :: String -> [String]
tokenize string = filter (not . null) $ 
  LS.splitOn " " [ C.toLower char | char <- string, char `notElem` ",.?!-:;\"\'" ]

-- Turns an array of datapoints into a CountVector of words and counts
vectorize :: [Datapoint] -> CountVector
vectorize datapoints = M.fromListWith (+) [ (token, 1.0) | token <- concatMap (tokenize . fst) datapoints ]

-- Train specified model by tokenizing and storing training data
train :: Classifier -> [Datapoint] -> Classifier
train (Classifier vocab pos neg counts) datapoints = Classifier
  (M.unionWith (+) vocab vectorized) 
  (M.unionWith (+) pos vectorizedPos) 
  (M.unionWith (+) neg vectorizedNeg)
  (M.unionWith (+) counts $ M.fromListWith (+) [
    (Pos, fromIntegral $ length filteredPos),
    (Neg, fromIntegral $ length filteredNeg)
  ]) 
  where 
      vectorized = vectorize datapoints
      filteredPos = filter ((== Pos) . snd) datapoints
      filteredNeg = filter ((== Neg) . snd) datapoints
      vectorizedPos = vectorize filteredPos
      vectorizedNeg = vectorize filteredNeg

-- Test a model and return its accurary
test :: Classifier -> [Datapoint] -> Double
test = undefined

-- Classify a string
classify :: Classifier -> String -> Sentiment
classify model string
  | max pos neg == pos = Pos
  | max pos neg == neg = Neg
  where
    pos = classify' model Pos string
    neg = classify' model Neg string

-- Calculate P(c|d), probability for any class given a document,
-- using log probabilities to prevent underflow
classify' :: Classifier -> Sentiment -> String -> Double
classify' model c string =  (log pc) + (sum pwc)
  where 
    pc = probClass model c
    pwc = map (\w -> log $ probWord model c w) $ tokenize string

-- Calculate P(c), probability for any class
probClass :: Classifier -> Sentiment -> Double
probClass model c = classCount / totalCount
  where
    classCount = M.findWithDefault 0.0 c $ counts model
    totalCount = sum $ M.elems $ counts model

-- Calculate P(w|c), probability for any word given a class
probWord :: Classifier -> Sentiment -> String -> Double
probWord model c word  
  | c == Pos = (1.0 + (M.findWithDefault 0.0 word $ positive model)) / (vocab + (sum (M.elems $ positive model)))
  | c == Neg = (1.0 + (M.findWithDefault 0.0 word $ negative model)) / (vocab + (sum (M.elems $ negative model)))
  | otherwise = error "undefined class"
  where vocab = fromIntegral (length (M.keys (vocabulary model)))
