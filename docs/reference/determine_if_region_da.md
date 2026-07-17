# Determine whether each cell is in a region of differential abundance.

`determine_if_region_da()` takes vectors of p-values, observed scores,
and the null distribution of scores and uses the Benjamini–Yekutieli
procedure to determine whether a cell is in a region of differential
abundance.

## Usage

``` r
determine_if_region_da(p_vals, scores, null_dist, alpha)
```

## Arguments

- p_vals:

  Numeric vector of p-values.

- scores:

  Numeric vector containing observed output of Dawnn.

- null_dist:

  Numeric vector containing the null distribution of scores.

- alpha:

  Numeric target false discovery rate supplied to the
  Benjamini–Yekutieli procedure.

## Value

Boolean vector containing Dawnn's verdict for each cell.

## Examples

``` r
if (FALSE) { # \dontrun{
determine_if_region_da(p_vals = p_value_vector, null_dist = null_scores,
alpha = 0.2)
} # }
```
