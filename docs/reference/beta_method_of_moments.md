# Estimate the parameters of a beta distribution using the method of moments.

Estimate the parameters of a beta distribution using the method of
moments.

## Usage

``` r
beta_method_of_moments(data)
```

## Arguments

- data:

  Vector of numbers to which for which to estimate the parameters.

## Value

A list containing the two parameters of the fitted beta ditribution.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
beta_sample <- rbeta(10000, shape1 = 2, shape2 = 5)
beta_method_of_moments(beta_sample)
# $alpha
# [1] 1.982009
#
# $beta
# [1] 4.942666
} # }
```
