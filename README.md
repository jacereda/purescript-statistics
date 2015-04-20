# Module Documentation

## Module Math.Statistics

#### `maximum`

``` purescript
maximum :: Sample -> Maybe Point
```

Maximum value.

#### `minimum`

``` purescript
minimum :: Sample -> Maybe Point
```

Minimum value.

#### `mean`

``` purescript
mean :: Sample -> Maybe Point
```

Mean.

#### `harmean`

``` purescript
harmean :: Sample -> Maybe Point
```

Harmonic mean.

#### `geomean`

``` purescript
geomean :: Sample -> Maybe Point
```

Geometric mean.

#### `median`

``` purescript
median :: Sample -> Maybe Point
```

Median.

#### `modes`

``` purescript
modes :: Sample -> [Tuple Int Point]
```

Modes returns a sorted list of modes in descending order.

#### `mode`

``` purescript
mode :: Sample -> Maybe Point
```

Mode returns the mode of the list.

#### `centralMoment`

``` purescript
centralMoment :: Int -> Sample -> Maybe Number
```

Central moments.

#### `range`

``` purescript
range :: Sample -> Maybe Number
```

Range.

#### `stddev`

``` purescript
stddev :: Sample -> Maybe Number
```

Standard deviation of sample.

#### `stddevp`

``` purescript
stddevp :: Sample -> Maybe Number
```

Standard deviation of population.

#### `pvar`

``` purescript
pvar :: Sample -> Maybe Number
```

Population variance.

#### `var`

``` purescript
var :: Sample -> Maybe Number
```

Sample variance.

#### `iqr`

``` purescript
iqr :: Sample -> Maybe Number
```

Interquartile range.

#### `skew`

``` purescript
skew :: Sample -> Maybe Number
```

Calculate skew.

#### `pearsonSkew`

``` purescript
pearsonSkew :: Sample -> Maybe Number
```

Calculates pearson skew.

#### `covar`

``` purescript
covar :: Sample -> Sample -> Maybe Number
```

Sample Covariance.

#### `covMatrix`

``` purescript
covMatrix :: [Sample] -> Maybe [[Number]]
```

Covariance matrix.

#### `pearson`

``` purescript
pearson :: Sample -> Sample -> Maybe Number
```

Pearson's product-moment correlation coefficient.

#### `linreg`

``` purescript
linreg :: Sample -> Sample -> Maybe (Tuple3 Number Number Number)
```

Least-squares linear regression of /y/ against /x/ for a
collection of (/x/, /y/) data, in the form of (/b0/, /b1/, /r/)
where the regression is /y/ = /b0/ + /b1/ * /x/ with Pearson
coefficient /r/.

#### `devsq`

``` purescript
devsq :: Sample -> Maybe Number
```

Returns the sum of square deviations from their sample mean.


## Module Math.Statistics.Types

#### `Int`

``` purescript
type Int = Number
```


#### `Point`

``` purescript
type Point = Number
```


#### `Sample`

``` purescript
type Sample = [Point]
```



## Module Math.Statistics.Unsafe

#### `square`

``` purescript
square :: Number -> Number
```


#### `div`

``` purescript
div :: Int -> Int -> Int
```


#### `even`

``` purescript
even :: Int -> Boolean
```


#### `maximum`

``` purescript
maximum :: Sample -> Point
```

Maximum value.

#### `minimum`

``` purescript
minimum :: Sample -> Point
```

Minimum value.

#### `mean`

``` purescript
mean :: Sample -> Point
```

Mean.

#### `harmean`

``` purescript
harmean :: Sample -> Point
```

Harmonic mean.

#### `geomean`

``` purescript
geomean :: Sample -> Point
```

Geometric mean.

#### `median`

``` purescript
median :: Sample -> Point
```

Median.

#### `modes`

``` purescript
modes :: Sample -> [Tuple Int Point]
```

Modes returns a sorted list of modes in descending order.

#### `mode`

``` purescript
mode :: Sample -> Point
```

Mode returns the mode of the non-empty list.

#### `skew`

``` purescript
skew :: Sample -> Number
```

Calculate skew.

#### `pearsonSkew`

``` purescript
pearsonSkew :: Sample -> Number
```

Calculates pearson skew.

#### `stddev`

``` purescript
stddev :: Sample -> Number
```

Standard deviation of sample.

#### `stddevp`

``` purescript
stddevp :: Sample -> Number
```

Standard deviation of population.

#### `var`

``` purescript
var :: Sample -> Number
```

Sample variance.

#### `pvar`

``` purescript
pvar :: Sample -> Number
```

Population variance.

#### `centralMoment`

``` purescript
centralMoment :: Int -> Sample -> Number
```

Central moments.

#### `range`

``` purescript
range :: Sample -> Number
```

Range.

#### `avgdev`

``` purescript
avgdev :: Sample -> Number
```

Average deviation.

#### `iqr`

``` purescript
iqr :: Sample -> Number
```

Interquartile range.

#### `iqr'`

``` purescript
iqr' :: Sample -> Number
```

Interquartile range for sorted data.

#### `kurt`

``` purescript
kurt :: Sample -> Number
```

Kurtosis.

#### `quantile`

``` purescript
quantile :: Int -> Sample -> Number
```

Arbitrary quantile q of an unsorted list.  The quantile /q/ of /N/
data points is the point whose (zero-based) index in the sorted
data set is closest to /q(N-1)/.

#### `quantile'`

``` purescript
quantile' :: Int -> Sample -> Number
```

As 'quantile' specialized for sorted data.

#### `covMatrix`

``` purescript
covMatrix :: [Sample] -> [[Number]]
```

Covariance matrix.

#### `pearson`

``` purescript
pearson :: Sample -> Sample -> Number
```

Pearson's product-moment correlation coefficient.

#### `covar`

``` purescript
covar :: Sample -> Sample -> Number
```

Sample Covariance.

#### `linreg`

``` purescript
linreg :: Sample -> Sample -> Tuple3 Number Number Number
```

Least-squares linear regression of /y/ against /x/ for a
collection of (/x/, /y/) data, in the form of (/b0/, /b1/, /r/)
where the regression is /y/ = /b0/ + /b1/ * /x/ with Pearson
coefficient /r/

#### `devsq`

``` purescript
devsq :: Sample -> Number
```

Returns the sum of square deviations from their sample mean.



