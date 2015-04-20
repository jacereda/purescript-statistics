module Test.Main where

import Debug.Trace
import Test.QuickCheck
import Control.Monad.Eff
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested(Tuple3(..), tuple3)
import Data.Array(length)
import Math(abs)

import Math.Statistics


(~=) :: Maybe Number -> Maybe Number -> Boolean
(~=) (Just x) (Just y) = abs (x - y) < 0.000001
(~=) Nothing Nothing = true
(~=) _ _ = false

infix 4 ~=

tst :: forall e. String -> Eff (trace :: Trace |e) Unit
tst x = do
  trace $ "Testing " ++ x

main = do
  tst "maximum"
  assert $ maximum [] == Nothing
  assert $ maximum [1] == Just 1
  assert $ maximum [1,2] == Just 2
  assert $ maximum [2,1] == Just 2

  tst "minimum"
  assert $ minimum [] == Nothing
  assert $ minimum [1] == Just 1
  assert $ minimum [1,2] == Just 1
  assert $ minimum [2,1] == Just 1

  tst "mean"
  assert $ mean [] == Nothing
  assert $ mean [1] == Just 1
  assert $ mean [4, 36, 45, 50, 75] ~= Just 42

  tst "harmean"
  assert $ harmean [] == Nothing
  assert $ harmean [1] == Just 1
  assert $ harmean [1,2,4] == Just (12 / 7)
  assert $ harmean [1,0,1] == Nothing
  assert $ harmean [4, 36, 45, 50, 75] ~= Just 15

  tst "geomean"
  assert $ geomean [] == Nothing
  assert $ geomean [1] == Just 1
  assert $ geomean [4, 36, 45, 50, 75] ~= Just 30

  tst "median"
  assert $ median [] == Nothing
  assert $ median [1] == Just 1
  assert $ median [3, 3, 5, 9, 11] ~= Just 5
  assert $ median [3, 5, 7, 9] ~= Just 6

  tst "modes"
  assert $ modes [] == []
  assert $ modes [1] == [Tuple 1 1]
  assert $ modes [1, 1, 2] == [Tuple 2 1, Tuple 1 2]
  assert $ modes [2, 1, 1] == [Tuple 2 1, Tuple 1 2]

  tst "mode"
  assert $ mode [] == Nothing
  assert $ mode [1] == Just 1
  assert $ mode [1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17] ~= Just 6
  assert $ mode [1, 1, 2, 4, 4] ~= Just 1

  tst "centralMoment"
  assert $ centralMoment 0 [] == Nothing
  assert $ centralMoment 0 [1] == Just 1
  assert $ centralMoment 1 [1] == Just 0
  quickCheck \xs -> centralMoment 2 xs ~= pvar xs

  tst "pvar"
  assert $ pvar [] == Nothing
  assert $ pvar [1] == Just 0
  assert $ pvar [1,2] == Just (1/4)
  assert $ pvar [1,2,3] == Just (2/3)

  tst "var"
  assert $ var [] == Nothing
  assert $ var [1] == Nothing
  assert $ var [1,2] == Just (1/2)
  assert $ var [1,2,3] == Just 1

  tst "range"
  assert $ range [] == Nothing
  assert $ range [1] == Just 0

  tst "stddev"
  assert $ stddev [] == Nothing
  assert $ stddev [1] == Nothing
  assert $ stddev [1,1] == Just 0

  tst "stddevp"
  assert $ stddevp [] == Nothing
  assert $ stddevp [1] == Just 0
  assert $ stddevp [1,1] == Just 0

  tst "iqr"
  assert $ iqr [] == Nothing
  assert $ iqr [1] == Just 0
  assert $ iqr [102, 104, 105, 107, 108, 109, 110, 112, 115, 116, 118] == Just 10
  assert $ iqr [5, 8, 4, 4, 6, 3, 8] == Just 4

  tst "covar"
  assert $ covar [] [] == Nothing
  assert $ covar [1] [1] == Nothing
  assert $ covar [1,2] [2,4] == Just 1
  assert $ covar [2.1, 2.5, 3.6, 4.0] [8, 10, 12, 14] == Just (6.8 / 3)
  assert $ covar [2.1, 2.5, 4.0, 3.6] [8, 12, 14, 10] == Just (4.6 / 3)

  tst "covMatrix"
  assert $ covMatrix [] == Nothing
  assert $ covMatrix [ [1] ] == Nothing
  assert $ covMatrix [ [1], [] ] == Nothing
  assert $ covMatrix [ [1,2], [1] ] == Nothing
  assert $ covMatrix [ [1,2]
                     , [1,2]
                     ] == Just [ [0.5,0.5], [0.5,0.5] ]
  assert $ covMatrix [ [0, 1, 2]
                     , [2, 1, 0]
                     ] == Just [ [1, (-1)], [(-1), 1] ]

  tst "pearson"
  assert $ pearson [] [] == Nothing
  assert $ pearson [1] [1] == Nothing
  assert $ pearson [1,1] [1,1] == Nothing
  assert $ pearson [56, 56, 65, 65, 50, 25, 87, 44, 35]
      [87, 91, 85, 91, 75, 28, 122, 66, 58] ~= Just 0.966194

  tst "skew"
  assert $ skew [] == Nothing
  assert $ skew [1] == Nothing
  assert $ skew [1,1] == Nothing
  assert $ skew [1,2] == Just 0
  assert $ skew [1,2,1] ~= Just 0.707107

  tst "pearsonSkew"
  assert $ pearsonSkew [] == Nothing
  assert $ pearsonSkew [1] == Nothing
  assert $ pearsonSkew [1,1] == Nothing
  assert $ pearsonSkew [1,2] == Just 0
  assert $ pearsonSkew [1,2,1] ~= Just 1.732051

  tst "linreg"
  assert $ linreg [] [] == Nothing
  assert $ linreg [1] [1] == Nothing
  assert $ linreg [1, 2] [2, 4] == Just (tuple3 0 2 1)

  tst "devsq"
  assert $ devsq [] == Nothing
  assert $ devsq [1] == Just 0
  assert $ devsq [1,2,3,4,6] == Just 14.8



assert :: Boolean -> QC Unit
assert = quickCheck' 1
