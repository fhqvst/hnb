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

-- Datapoint and CountVector
type Datapoint = (String, Sentiment)
type CountVector = M.Map String Double

-- The model
data Classifier = Classifier { 
  vocabulary :: CountVector,
  positive :: CountVector,
  negative :: CountVector,
  counts :: M.Map Sentiment Double
}

empty :: Classifier
empty = Classifier M.empty M.empty M.empty M.empty

sentiment :: Sentiment -> Sentiment
sentiment s = s

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

test :: Classifier -> [Datapoint] -> Double
test = undefined

classify :: Classifier -> Sentiment -> String -> Double
classify model c string = pc * (product pwc)
  where 
    pc = probClass model c
    pwc = map (\w -> probWord model c w) $ tokenize string

-- Returns P(C)
probClass :: Classifier -> Sentiment -> Double
probClass model c = classCount / totalCount
  where
    classCount = M.findWithDefault 0.0 c $ counts model
    totalCount = sum $ M.elems $ counts model

probWord :: Classifier -> Sentiment -> String -> Double
probWord model c word  
  | c == Pos = (M.findWithDefault 0.0 word $ positive model) / (sum (M.elems $ positive model))
  | c == Neg = (M.findWithDefault 0.0 word $ negative model) / (sum (M.elems $ negative model))
  | otherwise = error "undefined class"
