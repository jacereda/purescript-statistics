# Module Documentation

## Module Math.Statistics

#### `maximum`

``` purescript
maximum :: Sample -> Maybe Number
```

Maximum value.

#### `minimum`

``` purescript
minimum :: Sample -> Maybe Number
```

Minimum value.

#### `mean`

``` purescript
mean :: Sample -> Maybe Number
```

Mean.

#### `harmean`

``` purescript
harmean :: Sample -> Maybe Number
```

Harmonic mean.

#### `geomean`

``` purescript
geomean :: Sample -> Maybe Number
```

Geometric mean.

#### `median`

``` purescript
median :: Sample -> Maybe Number
```

Median.

#### `modes`

``` purescript
modes :: Sample -> [Tuple Number Number]
```

Modes returns a sorted list of modes in descending order.

#### `mode`

``` purescript
mode :: Sample -> Maybe Number
```

Mode returns the mode of the list.

#### `centralMoment`

``` purescript
centralMoment :: Sample -> Number -> Maybe Number
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


#### `Sample`

``` purescript
type Sample = [Number]
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
maximum :: [Number] -> Number
```

Maximum value.

#### `minimum`

``` purescript
minimum :: [Number] -> Number
```

Minimum value.

#### `mean`

``` purescript
mean :: [Number] -> Number
```

Mean.

#### `harmean`

``` purescript
harmean :: [Number] -> Number
```

Harmonic mean.

#### `geomean`

``` purescript
geomean :: [Number] -> Number
```

Geometric mean.

#### `median`

``` purescript
median :: [Number] -> Number
```

Median.

#### `modes`

``` purescript
modes :: [Number] -> [Tuple Number Number]
```

Modes returns a sorted list of modes in descending order.

#### `mode`

``` purescript
mode :: [Number] -> Number
```

Mode returns the mode of the non-empty list.

#### `skew`

``` purescript
skew :: [Number] -> Number
```

Calculate skew.

#### `pearsonSkew`

``` purescript
pearsonSkew :: [Number] -> Number
```

Calculates pearson skew.

#### `stddev`

``` purescript
stddev :: [Number] -> Number
```

Standard deviation of sample.

#### `stddevp`

``` purescript
stddevp :: [Number] -> Number
```

Standard deviation of population.

#### `var`

``` purescript
var :: [Number] -> Number
```

Sample variance.

#### `pvar`

``` purescript
pvar :: [Number] -> Number
```

Population variance.

#### `centralMoment`

``` purescript
centralMoment :: [Number] -> Number -> Number
```

Central moments.

#### `range`

``` purescript
range :: [Number] -> Number
```

Range.

#### `avgdev`

``` purescript
avgdev :: [Number] -> Number
```

Average deviation.

#### `iqr`

``` purescript
iqr :: [Number] -> Number
```

Interquartile range.

#### `iqr'`

``` purescript
iqr' :: [Number] -> Number
```

Interquartile range for sorted data.

#### `kurt`

``` purescript
kurt :: [Number] -> Number
```

Kurtosis.

#### `quantile`

``` purescript
quantile :: Number -> [Number] -> Number
```

Arbitrary quantile q of an unsorted list.  The quantile /q/ of /N/
data points is the point whose (zero-based) index in the sorted
data set is closest to /q(N-1)/.

#### `quantileAsc`

``` purescript
quantileAsc :: Number -> [Number] -> Number
```

As 'quantile' specialized for sorted data.

#### `covMatrix`

``` purescript
covMatrix :: [[Number]] -> [[Number]]
```

Covariance matrix.

#### `pearson`

``` purescript
pearson :: [Number] -> [Number] -> Number
```

Pearson's product-moment correlation coefficient.

#### `covar`

``` purescript
covar :: [Number] -> [Number] -> Number
```

Sample Covariance.

#### `linreg`

``` purescript
linreg :: [Number] -> [Number] -> Tuple3 Number Number Number
```

Least-squares linear regression of /y/ against /x/ for a
collection of (/x/, /y/) data, in the form of (/b0/, /b1/, /r/)
where the regression is /y/ = /b0/ + /b1/ * /x/ with Pearson
coefficient /r/

#### `devsq`

``` purescript
devsq :: [Number] -> Number
```

Returns the sum of square deviations from their sample mean.



