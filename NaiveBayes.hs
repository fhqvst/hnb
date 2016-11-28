import qualified Data.Map as M

data Sentiment = Pos | Neg deriving (Eq, Show)

type Datapoint = (String, Sentiment)

type CountVector = M.Map String Double

data NaiveBayes = NaiveBayes { 
  vocabulary :: CountVector,
  positive :: CountVector,
  negative :: CountVector
}

mapFst :: (a -> b) -> [(a, a)] -> [(b, a)]
mapFst f t = map (\(x, y) -> (f x, y)) t

-- Tokenize strings into an array of tokens
tokenize :: String -> [String]
tokenize string = filter (not . null) $ 
  splitOn " " [ toLower char | char <- string, char `notElem` ",.?!-:;\"\'" ]

-- Train specified model by tokenizing and storing training data
train :: NaiveBayes -> [Datapoint] -> NaiveBayes
train (NaiveBayes vocab pos neg) datapoints = NaiveBayes
  (vocab ++ tokenized) pos neg
    where tokenized = mapFst tokenize datapoints 

test :: NaiveBayes -> [Datapoint] -> Double
test = undefined

classify :: NaiveBayes -> String -> Double
classify = undefined
