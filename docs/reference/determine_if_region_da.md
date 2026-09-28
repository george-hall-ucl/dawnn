# Determine whether each cell is in a region of differential abundance.

`determine_if_region_da()` takes a vector of p-values and uses the
Benjamini–Yekutieli procedure to determine whether a cell is in a region
of differential abundance.

## Usage

``` r
determine_if_region_da(p_vals, alpha)
```

## Arguments

- p_vals:

  Numeric vector of p-values.

- alpha:

  Numeric target false discovery rate supplied to the
  Benjamini–Yekutieli procedure.

## Value

Boolean vector containing Dawnn's verdict for each cell.
