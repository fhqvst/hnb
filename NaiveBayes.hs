module NaiveBayes (
  Classifier,
  Sentiment,
  vocabulary,
  positive,
  negative,
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
  deriving (Eq, Show)

pos = Pos
neg = Neg

-- Datapoint and CountVector
type Datapoint = (String, Sentiment)
type CountVector = M.Map String Double

-- The model
data Classifier = Classifier { 
  vocabulary :: CountVector,
  positive :: CountVector,
  negative :: CountVector
}

empty :: Classifier
empty = Classifier M.empty M.empty M.empty

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
train (Classifier vocab pos neg) datapoints = Classifier
  (M.unionWith (+) vocab vectorized) (M.unionWith (+) pos vectorizedPos) (M.unionWith (+) neg vectorizedNeg)
    where 
      vectorized = vectorize datapoints
      vectorizedPos = vectorize $ filter ((== Pos) . snd) datapoints
      vectorizedNeg = vectorize $ filter ((== Neg) . snd) datapoints

test :: Classifier -> [Datapoint] -> Double
test = undefined

classify :: Classifier -> String -> Double
classify model string = 1.0
