module Math.Statistics( maximum
                      , minimum
                      , mean
                      , harmean
                      , geomean
                      , median
                      , modes
                      , mode
                      , centralMoment
                      , pvar
                      , var
                      , iqr
                      , covar
                      , covMatrix
                      , range
                      , stddev
                      , stddevp
                      , pearson
                      , skew
                      , pearsonSkew
                      , linreg
                      , devsq
                      ) where

import Global(nan, isNaN, infinity, isFinite)
import Data.Tuple
import Data.Tuple.Nested
import Data.Maybe
import Data.Either
import Data.Array(sort, length,map)
import Data.Traversable(traverse)
import Data.Foldable(all)
import Control.MonadPlus(guard)
import Debug.Foreign(fspy)

import Math.Statistics.Types
import qualified Math.Statistics.Unsafe as U

chknan :: Number -> Maybe Number
chknan r = if isNaN r then Nothing else Just r

chkinf :: Number -> Maybe Number
chkinf r = if isFinite r then Just r else Nothing

-- | Maximum value.
maximum :: Sample -> Maybe Point
maximum = chkinf <<< U.maximum

-- | Minimum value.
minimum :: Sample -> Maybe Point
minimum = chkinf <<< U.minimum

-- | Mean.
mean :: Sample -> Maybe Point
mean = chknan <<< U.mean

-- | Harmonic mean.
harmean :: Sample -> Maybe Point
harmean xs = do
  guard $ length xs > 0
  guard $ all (>0) xs
  return $ U.harmean xs

-- | Geometric mean.
geomean :: Sample -> Maybe Point
geomean = chknan <<< U.geomean

-- | Median.
median :: Sample -> Maybe Point
median = chknan <<< U.median

-- | Modes returns a sorted list of modes in descending order.
modes :: Sample -> [Tuple Int Point]
modes = U.modes

-- | Mode returns the mode of the list.
mode :: Sample -> Maybe Point
mode xs = do
  guard $ length xs > 0
  return $ U.mode xs

-- | Central moments.
centralMoment :: Int -> Sample -> Maybe Number
centralMoment n = chknan <<< U.centralMoment n

-- | Range.
range :: Sample -> Maybe Number
range = chkinf <<< U.range

-- | Average deviation.
avgdev :: Sample -> Maybe Number
avgdev = chknan <<< U.avgdev

-- | Standard deviation of sample.
stddev :: Sample -> Maybe Number
stddev xs = do
    guard $ length xs > 1
    return $ U.stddev xs

-- | Standard deviation of population.
stddevp :: Sample -> Maybe Number
stddevp = chknan <<< U.stddevp

-- | Population variance.
pvar :: Sample -> Maybe Number
pvar = chknan <<< U.pvar

-- | Sample variance.
var :: Sample -> Maybe Number
var xs = do
  guard $ length xs > 1
  return $ U.var xs

-- | Interquartile range.
iqr :: Sample -> Maybe Number
iqr = chkinf <<< U.iqr

-- | Interquartile range for sorted data.
iqr' :: Sample -> Maybe Number
iqr' = chkinf <<< U.iqr'

-- | Kurtosis.
kurt :: Sample -> Maybe Number
kurt = chknan <<< U.kurt

-- | Arbitrary quantile q of an unsorted list.  The quantile /q/ of /N/
-- | data points is the point whose (zero-based) index in the sorted
-- | data set is closest to /q(N-1)/.
quantile :: Int -> Sample -> Either String Number
quantile q = quantile' q <<< sort

-- | As 'quantile' specialized for sorted data.
quantile' :: Int -> Sample -> Either String Number
quantile' _ [] = Left "quantile on empty list"
quantile' q _ | q < 0 || q > 1 = Left "quantile out of range"
quantile' q xs = if isNaN qa then Left "bad quantile index" else Right qa
  where qa = U.quantile' q xs

-- | Calculate skew.
skew :: Sample -> Maybe Number
skew = chknan <<< U.skew

-- | Calculates pearson skew.
pearsonSkew :: Sample -> Maybe Number
pearsonSkew = chknan <<< U.pearsonSkew

-- | Sample Covariance.
covar :: Sample -> Sample -> Maybe Number
covar xs ys = do
  let lxs = length xs
  guard $ lxs > 1
  guard $ length ys == lxs
  return $ U.covar xs ys

-- | Covariance matrix.
covMatrix :: [Sample] -> Maybe [[Number]]
covMatrix xs = do
  guard $ length xs > 1
  guard $ same $ map length xs
  return $ U.covMatrix xs
  where same (x:xs) = all (==x) xs

-- | Pearson's product-moment correlation coefficient.
pearson :: Sample -> Sample -> Maybe Number
pearson xs = chknan <<< U.pearson xs

-- | Least-squares linear regression of /y/ against /x/ for a
-- | collection of (/x/, /y/) data, in the form of (/b0/, /b1/, /r/)
-- | where the regression is /y/ = /b0/ + /b1/ * /x/ with Pearson
-- | coefficient /r/.
linreg :: Sample -> Sample -> Maybe (Tuple3 Number Number Number)
linreg xs ys = do
  let lxs = length xs
  guard $ lxs > 1
  guard $ length ys == lxs
  return $ U.linreg xs ys

-- | Returns the sum of square deviations from their sample mean.
devsq :: Sample -> Maybe Number
devsq xs = do
  guard $ length xs > 0
  return $ U.devsq xs
