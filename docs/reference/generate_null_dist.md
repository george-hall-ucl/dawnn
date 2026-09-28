# Generate a null distribution of P(Condition_1) estimates.

`generate_null_dist()` shuffles the sample labels three times and
returns the estimates of P(Condition_1) for each shuffled dataset.

## Usage

``` r
generate_null_dist(
  cells,
  model,
  label_names,
  label_pos_lfc,
  verbosity,
  da_mode = c("lda", "gda")
)
```

## Arguments

- cells:

  Seurat object containing the dataset.

- model:

  Loaded neural network model to use.

- label_names:

  String containing the name of the meta.data slot in `cells` containing
  the labels of each cell.

- label_pos_lfc:

  String containing the name of the label associated with positive
  log-fold change.

- verbosity:

  Integer how much output to print. 0: silent; 1: normal output; 2:
  display messages from predict() function.

- da_mode:

  String containing the type of differential abundance being sought,
  either "lda" (local DA) or "gda" (global DA).

## Value

A vector containing a null distribution of Dawnn's model outputs for
shuffled sample labels.
