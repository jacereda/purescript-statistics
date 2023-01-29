module Math.Statistics.Unsafe where

import Prelude hiding (max, min)

import Data.Array (length, group, sort, sortBy, zipWith, drop, take, unsafeIndex)
import Data.Array.NonEmpty as NE
import Data.Array.Partial as U
import Data.Foldable (foldl, sum, product)
import Data.Int (even, toNumber, round)
import Data.Number (nan, infinity, sqrt, pow, max, min, abs)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (Tuple3, tuple3)
import Math.Statistics.Types (Point, Sample)
import Partial.Unsafe (unsafePartial)

square :: Number -> Number
square x = x * x

-- | Maximum value.
maximum :: Sample -> Point
maximum = foldl max (-infinity)

-- | Minimum value.
minimum :: Sample -> Point
minimum = foldl min infinity

-- | Mean.
mean :: Sample -> Point
mean xs = sum xs / toNumber (length xs)

-- | Harmonic mean.
harmean :: Sample -> Point
harmean xs = toNumber (length xs) / (sum $ map (1.0 / _) xs)

-- | Geometric mean.
geomean :: Sample -> Point
geomean xs = (product xs) `pow` (1.0 / toNumber (length xs))

-- | Median.
median :: Sample -> Point
median xs = m $ sort xs
  where
  n = length xs
  i = n / 2
  m x | even n = mean $ take 2 $ drop (i - 1) x
  m x = unsafePartial $ U.head $ drop i x

-- | Sorted array of modes in descending order.
modes :: Sample -> Array (Tuple Int Point)
modes = sortBy (comparing $ negate <<< fst)
  <<< map (\x -> Tuple (NE.length x) (NE.head x))
  <<< group
  <<< sort
  where
  comparing p x y = compare (p x) (p y)

-- | Mode for the non-empty sample.
mode :: Sample -> Point
mode = snd <<< unsafePartial U.head <<< modes

-- | Calculate skew.
skew :: Sample -> Number
skew xs = (centralMoment 3 xs) / ((pvar xs) `pow` 1.5)

-- | Calculates pearson skew.
pearsonSkew :: Sample -> Number
pearsonSkew xs = 3.0 * (mean xs - median xs) / stddev xs

-- | Standard deviation of sample.
stddev :: Sample -> Number
stddev = sqrt <<< var

-- | Standard deviation of population.
stddevp :: Sample -> Number
stddevp = sqrt <<< pvar

-- | Sample variance.
var :: Sample -> Number
var = var' 1

-- | Population variance.
pvar :: Sample -> Number
pvar = var' 0

var' :: Int -> Sample -> Number
var' n xs = sum (map d xs) / toNumber (length xs - n)
  where
  m = mean xs
  d x = square $ x - m

-- | Central moments.
centralMoment :: Int -> Sample -> Number
centralMoment 1 _ = 0.0
centralMoment r xs = (sum (map (\x -> (x - m) `pow` (toNumber r)) xs)) / (toNumber n)
  where
  m = mean xs
  n = length xs

-- | Range.
range :: Sample -> Number
range xs = maximum xs - minimum xs

-- | Average deviation.
avgdev :: Sample -> Number
avgdev xs = mean $ map (\x -> abs (x - m)) xs
  where
  m = mean xs

-- | Interquartile range.
iqr :: Sample -> Number
iqr = iqr' <<< sort

-- | Interquartile range for sorted data.
iqr' :: Sample -> Number
iqr' xs = range $ take (length xs - 2 * q) $ drop q xs
  where
  q = ((length xs) - 1) / 4

-- | Kurtosis.
kurt :: Sample -> Number
kurt xs = ((centralMoment 4 xs) / square (pvar xs)) - 3.0

-- | Arbitrary quantile q of an unsorted list.  The quantile /q/ of /N/
-- | data points is the point whose (zero-based) index in the sorted
-- | data set is closest to /q(N-1)/.
quantile :: Number -> Sample -> Number
quantile q = quantile' q <<< sort

-- | As 'quantile' specialized for sorted data.
quantile' :: Number -> Sample -> Number
quantile' _ [] = nan
quantile' q xs =
  if between 0.0 1.0 q then unsafePartial $ unsafeIndex xs $ quantIndex $ length xs
  else nan
  where
  quantIndex :: Int -> Int
  quantIndex len = case round (q * toNumber (len - 1)) of
    idx | idx < 0 -> 0
    idx | idx >= len -> len - 1
    idx | otherwise -> idx

-- | Covariance matrix.
covMatrix :: Array Sample -> Array (Array Number)
covMatrix xs = do
  a <- xs
  pure $ do
    b <- xs
    pure $ covar a b

-- | Pearson's product-moment correlation coefficient.
pearson :: Sample -> Sample -> Number
pearson x y = covar x y / (stddev x * stddev y)

-- | Sample Covariance.
covar :: Sample -> Sample -> Number
covar xs ys = sum (zipWith (*) (map f1 xs) (map f2 ys)) / (toNumber $ n - 1)
  where
  n = length xs
  m1 = mean xs
  m2 = mean ys
  f1 = \x -> (x - m1)
  f2 = \x -> (x - m2)

-- | Least-squares linear regression of /y/ against /x/ for a
-- | collection of (/x/, /y/) data, in the form of (/b0/, /b1/, /r/)
-- | where the regression is /y/ = /b0/ + /b1/ * /x/ with Pearson
-- | coefficient /r/
linreg :: Sample -> Sample -> Tuple3 Number Number Number
linreg xs ys =
  let
    n = toNumber $ length xs
    sX = sum xs
    sY = sum ys
    sXX = sum $ map square xs
    sXY = sum $ zipWith (*) xs ys
    sYY = sum $ map square ys
    alpha = (sY - beta * sX) / n
    beta = (n * sXY - sX * sY) / (n * sXX - sX * sX)
    r = (n * sXY - sX * sY) /
      (sqrt $ (n * sXX - square sX) * (n * sYY - square sY))
  in
    tuple3 alpha beta r

-- | Sum of square deviations from their sample mean.
devsq :: Sample -> Number
devsq xs = sum $ map (\x -> square (x - m)) xs
  where
  m = mean xs
