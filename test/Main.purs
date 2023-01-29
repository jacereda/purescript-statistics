module Test.Main where

import Prelude
import Data.Foldable (all)
import Test.QuickCheck ((<?>), quickCheck)
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (tuple3)
import Data.Array (zip, length)
import Data.Number (abs)

import Math.Statistics (centralMoment, covMatrix, covar, devsq, geomean, harmean, iqr, linreg, maximum, mean, median, minimum, mode, modes, pearson, pearsonSkew, pvar, range, skew, stddev, stddevp, var)

class AlmostEq a where
  almostEq :: a -> a -> Boolean

infix 4 almostEq as ~=

instance almostEqNumber :: AlmostEq Number where
  almostEq x y = abs (x - y) < 0.000001

instance almostEqInt :: AlmostEq Int where
  almostEq x y = x == y

instance almostEqMaybe :: AlmostEq a => AlmostEq (Maybe a) where
  almostEq (Just x) (Just y) = x ~= y
  almostEq Nothing Nothing = true
  almostEq _ _ = false

instance almostEqTuple :: (AlmostEq a, AlmostEq b) => AlmostEq (Tuple a b) where
  almostEq (Tuple x y) (Tuple z t) = x ~= z && y ~= t

instance almostEqArray :: AlmostEq a => AlmostEq (Array a) where
  almostEq xs ys = length xs == length ys && all (\p -> fst p ~= snd p) (zip xs ys)

instance almostEqUnit :: AlmostEq Unit where
  almostEq _ _ = true

assertAlmostEquals :: forall a. AlmostEq a => Show a => a -> a -> Effect Unit
assertAlmostEquals a b = logShow $ a ~= b <?> show a <> " /= " <> show b

infix 2 assertAlmostEquals as ~==

tst :: String -> Effect Unit
tst x = do
  log $ "Testing " <> x

main :: Effect Unit
main = do
  tst "maximum"
  maximum [] ~== Nothing
  maximum [ 1.0 ] ~== Just 1.0
  maximum [ 1.0, 2.0 ] ~== Just 2.0
  maximum [ 2.0, 1.0 ] ~== Just 2.0

  tst "minimum"
  minimum [] ~== Nothing
  minimum [ 1.0 ] ~== Just 1.0
  minimum [ 1.0, 2.0 ] ~== Just 1.0
  minimum [ 2.0, 1.0 ] ~== Just 1.0

  tst "mean"
  mean [] ~== Nothing
  mean [ 1.0 ] ~== Just 1.0
  mean [ 4.0, 36.0, 45.0, 50.0, 75.0 ] ~== Just 42.0

  tst "harmean"
  harmean [] ~== Nothing
  harmean [ 1.0 ] ~== Just 1.0
  harmean [ 1.0, 2.0, 4.0 ] ~== Just (12.0 / 7.0)
  harmean [ 1.0, 0.0, 1.0 ] ~== Nothing
  harmean [ 4.0, 36.0, 45.0, 50.0, 75.0 ] ~== Just 15.0

  tst "geomean"
  geomean [] ~== Nothing
  geomean [ 1.0 ] ~== Just 1.0
  geomean [ 4.0, 36.0, 45.0, 50.0, 75.0 ] ~== Just 30.0

  tst "median"
  median [] ~== Nothing
  median [ 1.0 ] ~== Just 1.0
  median [ 3.0, 3.0, 5.0, 9.0, 11.0 ] ~== Just 5.0
  median [ 3.0, 5.0, 7.0, 9.0 ] ~== Just 6.0

  tst "modes"
  modes [] ~== []
  modes [ 1.0 ] ~== [ Tuple 1 1.0 ]
  modes [ 1.0, 1.0, 2.0 ] ~== [ Tuple 2 1.0, Tuple 1 2.0 ]
  modes [ 2.0, 1.0, 1.0 ] ~== [ Tuple 2 1.0, Tuple 1 2.0 ]

  tst "mode"
  mode [] ~== Nothing
  mode [ 1.0 ] ~== Just 1.0
  mode [ 1.0, 3.0, 6.0, 6.0, 6.0, 6.0, 7.0, 7.0, 12.0, 12.0, 17.0 ] ~== Just 6.0
  mode [ 1.0, 1.0, 2.0, 4.0, 4.0 ] ~== Just 1.0

  tst "centralMoment"
  centralMoment 0 [] ~== Nothing
  centralMoment 0 [ 1.0 ] ~== Just 1.0
  centralMoment 1 [ 1.0 ] ~== Just 0.0
  quickCheck \xs -> centralMoment 2 xs ~= pvar xs

  tst "pvar"
  pvar [] ~== Nothing
  pvar [ 1.0 ] ~== Just 0.0
  pvar [ 1.0, 2.0 ] ~== Just (1.0 / 4.0)
  pvar [ 1.0, 2.0, 3.0 ] ~== Just (2.0 / 3.0)

  tst "var"
  var [] ~== Nothing
  var [ 1.0 ] ~== Nothing
  var [ 1.0, 2.0 ] ~== Just (1.0 / 2.0)
  var [ 1.0, 2.0, 3.0 ] ~== Just 1.0

  tst "range"
  range [] ~== Nothing
  range [ 1.0 ] ~== Just 0.0

  tst "stddev"
  stddev [] ~== Nothing
  stddev [ 1.0 ] ~== Nothing
  stddev [ 1.0, 1.0 ] ~== Just 0.0
  stddev
    [ 26.0
    , 28.0
    , 30.0
    , 37.0
    , 33.0
    , 30.0
    , 29.0
    , 39.0
    , 49.0
    , 31.0
    , 38.0
    , 36.0
    , 33.0
    , 24.0
    , 34.0
    , 40.0
    , 29.0
    , 41.0
    , 40.0
    , 29.0
    , 35.0
    , 44.0
    , 32.0
    , 45.0
    , 35.0
    , 26.0
    , 42.0
    , 36.0
    , 37.0
    , 35.0
    ] ~== Just 6.072455240154362

  tst "stddevp"
  stddevp [] ~== Nothing
  stddevp [ 1.0 ] ~== Just 0.0
  stddevp [ 1.0, 1.0 ] ~== Just 0.0

  tst "iqr"
  iqr [] ~== Nothing
  iqr [ 1.0 ] ~== Just 0.0
  iqr [ 102.0, 104.0, 105.0, 107.0, 108.0, 109.0, 110.0, 112.0, 115.0, 116.0, 118.0 ] ~== Just 10.0
  iqr [ 5.0, 8.0, 4.0, 4.0, 6.0, 3.0, 8.0 ] ~== Just 4.0

  tst "covar"
  covar [] ~== Nothing
  covar [ { x: 1.0, y: 1.0 } ] ~== Nothing
  covar [ { x: 1.0, y: 2.0 }, { x: 2.0, y: 4.0 } ] ~== Just 1.0
  covar [ { x: 2.1, y: 8.0 }, { x: 2.5, y: 10.0 }, { x: 3.6, y: 12.0 }, { x: 4.0, y: 14.0 } ] ~== Just (6.8 / 3.0)
  covar [ { x: 2.1, y: 8.0 }, { x: 2.5, y: 12.0 }, { x: 4.0, y: 14.0 }, { x: 3.6, y: 10.0 } ] ~== Just (4.6 / 3.0)

  tst "covMatrix"
  covMatrix [] ~== Nothing
  covMatrix [ [ 1.0 ] ] ~== Nothing
  covMatrix [ [ 1.0 ], [] ] ~== Nothing
  covMatrix [ [ 1.0, 2.0 ], [ 1.0 ] ] ~== Nothing
  covMatrix
    [ [ 1.0, 2.0 ]
    , [ 1.0, 2.0 ]
    ] ~== Just [ [ 0.5, 0.5 ], [ 0.5, 0.5 ] ]
  covMatrix
    [ [ 0.0, 1.0, 2.0 ]
    , [ 2.0, 1.0, 0.0 ]
    ] ~== Just [ [ 1.0, (-1.0) ], [ (-1.0), 1.0 ] ]

  tst "pearson"
  pearson [] ~== Nothing
  pearson [ { x: 1.0, y: 1.0 } ] ~== Nothing
  pearson [ { x: 1.0, y: 1.0 }, { x: 1.0, y: 1.0 } ] ~== Nothing
  pearson
    [ { x: 56.0, y: 87.0 }
    , { x: 56.0, y: 91.0 }
    , { x: 65.0, y: 85.0 }
    , { x: 65.0, y: 91.0 }
    , { x: 50.0, y: 75.0 }
    , { x: 25.0, y: 28.0 }
    , { x: 87.0, y: 122.0 }
    , { x: 44.0, y: 66.0 }
    , { x: 35.0, y: 58.0 }
    ] ~== Just 0.966194

  tst "skew"
  skew [] ~== Nothing
  skew [ 1.0 ] ~== Nothing
  skew [ 1.0, 1.0 ] ~== Nothing
  skew [ 1.0, 2.0 ] ~== Just 0.0
  skew [ 1.0, 2.0, 1.0 ] ~== Just 0.707107

  tst "pearsonSkew"
  pearsonSkew [] ~== Nothing
  pearsonSkew [ 1.0 ] ~== Nothing
  pearsonSkew [ 1.0, 1.0 ] ~== Nothing
  pearsonSkew [ 1.0, 2.0 ] ~== Just 0.0
  pearsonSkew [ 1.0, 2.0, 1.0 ] ~== Just 1.732051

  tst "linreg"
  linreg [] ~== Nothing
  linreg [ { x: 1.0, y: 1.0 } ] ~== Nothing
  linreg [ { x: 1.0, y: 2.0 }, { x: 2.0, y: 4.0 } ] ~== Just (tuple3 0.0 2.0 1.0)

  tst "devsq"
  devsq [] ~== Nothing
  devsq [ 1.0 ] ~== Just 0.0
  devsq [ 1.0, 2.0, 3.0, 4.0, 6.0 ] ~== Just 14.8
