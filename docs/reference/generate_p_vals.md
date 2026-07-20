# Generate p-values for observed Dawnn model outputs.

[`generate_null_dist()`](https://george-hall-ucl.github.io/dawnn/reference/generate_null_dist.md)
takes Dawnn model outputs and a null distribution and returns p-values
of the observed outputs.

## Usage

``` r
generate_p_vals(scores, null_dist)
```

## Arguments

- scores:

  Numeric vector containing observed output of Dawnn.

- null_dist:

  Numeric vector containing null distribution of scores.

## Value

Numeric vector containing a p-value for each cell, i.e. the probability
of observing at least such an extreme score for a cell given the beta
distribution fitted to the null distribution of scores.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_p_vals(scores = score_vect, null_dist = null_scores)
} # }
```
