module Math.Statistics.Unsafe where

import Global(nan, infinity)
import Math(sqrt, pow, max, min, abs, round)
import Data.Maybe
import Data.Tuple(Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested(Tuple3(..), tuple2, tuple3)
import Data.Foldable(foldl, foldr, sum, product)
import Data.Array(length, map, group, sort, sortBy, filter, zipWith, drop, take)
import qualified Data.Array.Unsafe as U
import Prelude.Unsafe(unsafeIndex)

import Math.Statistics.Types

square :: Number -> Number
square x = x * x

div :: Int -> Int -> Int
div x y = 0 .|. (x / y)

even :: Int -> Boolean
even x = x `div` 2 == x / 2

-- | Maximum value.
maximum :: [Number] -> Number
maximum = foldl max (-infinity)

-- | Minimum value.
minimum :: [Number] -> Number
minimum = foldl min infinity

-- | Mean.
mean :: [Number] -> Number
mean xs = sum xs / length xs

-- | Harmonic mean.
harmean :: [Number] -> Number
harmean xs = length xs / (sum $ map (1 /) xs)

-- | Geometric mean.
geomean :: [Number] -> Number
geomean xs = (product xs) `pow` (1 / length xs)

-- | Median.
median :: [Number] -> Number
median xs = m $ sort xs
  where n = length xs
        i = n `div` 2
        m x | even n = mean $ take 2 $ drop (i - 1) x
        m x = U.head  $ drop i x

-- | Modes returns a sorted list of modes in descending order.
modes :: [Number] -> [Tuple Number Number]
modes = sortBy (comparing $ negate <<< fst)
  <<< map (\x -> Tuple (length x) (U.head x))
  <<< (group <<< sort)
  where comparing p x y = compare (p x) (p y)

-- | Mode returns the mode of the non-empty list.
mode :: [Number] -> Number
mode = snd <<< U.head <<< modes

-- | Calculate skew.
skew :: [Number] -> Number
skew xs = (centralMoment xs 3) / ((pvar xs) `pow` 1.5)

-- | Calculates pearson skew.
pearsonSkew :: [Number] -> Number
pearsonSkew xs = 3 * (mean xs - median xs) / stddev xs

-- | Standard deviation of sample.
stddev :: [Number] -> Number
stddev = sqrt <<< var

-- | Standard deviation of population.
stddevp :: [Number] -> Number
stddevp = sqrt <<< pvar

-- | Sample variance.
var :: [Number] -> Number
var xs = (var' 0 0 0 xs) / (length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(n + 1)

-- | Population variance.
pvar :: [Number] -> Number
pvar xs = centralMoment xs 2

-- | Central moments.
centralMoment :: [Number] -> Number -> Number
centralMoment xs 1 = 0
centralMoment xs r = (sum (map (\x -> (x-m) `pow` r) xs)) / n
    where
      m = mean xs
      n = length xs

-- | Range.
range :: [Number] -> Number
range xs = maximum xs - minimum xs

-- | Average deviation.
avgdev :: [Number] -> Number
avgdev xs = mean $ map (\x -> abs(x - m)) xs
    where
      m = mean xs

-- | Interquartile range.
iqr :: [Number] -> Number
iqr = iqr' <<< sort

-- | Interquartile range for sorted data.
iqr' :: [Number] -> Number
iqr' xs = range $ take (length xs - 2*q) $ drop q xs
    where
      q = ((length xs)-1) `div` 4

-- | Kurtosis.
kurt :: [Number] -> Number
kurt xs = ((centralMoment xs 4) / square (pvar xs)) - 3

-- | Arbitrary quantile q of an unsorted list.  The quantile /q/ of /N/
-- | data points is the point whose (zero-based) index in the sorted
-- | data set is closest to /q(N-1)/.
quantile :: Number -> [Number] -> Number
quantile q = quantileAsc q <<< sort

-- | As 'quantile' specialized for sorted data.
quantileAsc :: Number -> [Number] -> Number
quantileAsc _ [] = nan
quantileAsc q xs = qa q xs
    where quantIndex :: Int -> Number -> Int
          quantIndex len q = case round $ q * (len - 1) of
                               idx | idx < 0    -> nan
                               idx | idx >= len -> nan
                               idx | otherwise  -> idx
          qa q xs | q < 0 || q > 1 = nan
          qa q xs = xs `unsafeIndex` (quantIndex (length xs) q)


-- | Covariance matrix.
covMatrix :: [[Number]] -> [[Number]]
covMatrix xs = do
  a <- xs
  return $ do
    b <- xs
    return $ covar a b

-- | Pearson's product-moment correlation coefficient.
pearson :: [Number] -> [Number] -> Number
pearson x y = covar x y / (stddev x * stddev y)

-- | Sample Covariance.
covar :: [Number] -> [Number] -> Number
covar xs ys = sum (zipWith (*) (map f1 xs) (map f2 ys)) / (n-1)
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
linreg :: [Number] -> [Number] -> Tuple3 Number Number Number
linreg xs ys = let n = length xs
                   sX = sum xs
                   sY = sum ys
                   sXX = sum $ map square xs
                   sXY = sum $ zipWith (*) xs ys
                   sYY = sum $ map square ys
                   alpha = (sY - beta * sX) / n
                   beta = (n * sXY - sX * sY) / (n * sXX - sX * sX)
                   r = (n * sXY - sX * sY) /
                    (sqrt $ (n * sXX - square sX) * (n * sYY - square sY))
                   in tuple3 alpha beta r

-- | Returns the sum of square deviations from their sample mean.
devsq :: [Number] -> Number
devsq xs = sum $ map (\x -> square (x-m)) xs
    where m = mean xs
