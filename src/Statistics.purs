module Math.Statistics
  ( avgdev
  , maximum
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
  , iqr'
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
  , kurt
  , quantile
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Array (sort, length)
import Data.Array.Partial as P
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.Number (isNaN, isFinite)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (Tuple3)
import Math.Statistics.Types (Point, Sample, XYSample)
import Math.Statistics.Unsafe as U
import Partial.Unsafe (unsafePartial)

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
  guard $ all (_ > 0.0) xs
  pure $ U.harmean xs

-- | Geometric mean.
geomean :: Sample -> Maybe Point
geomean = chknan <<< U.geomean

-- | Median.
median :: Sample -> Maybe Point
median = chknan <<< U.median

-- | Sorted array of modes in descending order.
modes :: Sample -> Array (Tuple Int Point)
modes = U.modes

-- | Mode.
mode :: Sample -> Maybe Point
mode xs = do
  guard $ length xs > 0
  pure $ U.mode xs

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
  pure $ U.stddev xs

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
  pure $ U.var xs

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
quantile :: Number -> Sample -> Either String Number
quantile q = quantile' q <<< sort

-- | As 'quantile' specialized for sorted data.
quantile' :: Number -> Sample -> Either String Number
quantile' _ [] = Left "quantile on empty list"
quantile' q _ | not (between 0.0 1.0 q) = Left "quantile out of range"
quantile' q xs = if isNaN qa then Left "bad quantile index" else Right qa
  where
  qa = U.quantile' q xs

-- | Calculate skew.
skew :: Sample -> Maybe Number
skew = chknan <<< U.skew

-- | Calculates pearson skew.
pearsonSkew :: Sample -> Maybe Number
pearsonSkew = chknan <<< U.pearsonSkew

-- | Sample Covariance.
covar :: XYSample -> Maybe Number
covar xys = do
  guard $ length xys > 1
  pure $ U.covar ((_.x) <$> xys) ((_.y) <$> xys)

-- | Covariance matrix.
covMatrix :: Array Sample -> Maybe (Array (Array Number))
covMatrix xs = do
  guard $ length xs > 1
  guard $ same $ map length xs
  pure $ U.covMatrix xs
  where
  same ls = all (_ == unsafePartial P.head ls) ls

-- | Pearson's product-moment correlation coefficient.
pearson :: XYSample -> Maybe Number
pearson xys = chknan $ U.pearson ((_.x) <$> xys) ((_.y) <$> xys)

-- | Least-squares linear regression of /y/ against /x/ for a
-- | collection of (/x/, /y/) data, in the form of (/b0/, /b1/, /r/)
-- | where the regression is /y/ = /b0/ + /b1/ * /x/ with Pearson
-- | coefficient /r/.
linreg :: XYSample -> Maybe (Tuple3 Number Number Number)
linreg xys = do
  guard $ length xys > 1
  pure $ U.linreg ((_.x) <$> xys) ((_.y) <$> xys)

-- | Sum of square deviations from their sample mean.
devsq :: Sample -> Maybe Number
devsq xs = do
  guard $ length xs > 0
  pure $ U.devsq xs
