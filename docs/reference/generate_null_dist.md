# Generate a null distribution of P(Condition_1) estimates.

`generate_null_dist()` shuffles the sample labels three times and
returns the estimates of P(Condition_1) for each shuffled dataset.

## Usage

``` r
generate_null_dist(
  cells,
  model,
  label_names,
  label_1,
  label_2,
  verbosity,
  da_mode
)
```

## Arguments

- cells:

  Seurat object containing the dataset.

- model:

  Loaded neural network model to use.

- label_names:

  String containing the name of the meta.data slot in \`cells'
  containing the labels of each cell.

- label_1:

  String containing the name of one of the labels.

- label_2:

  String containing the name of the other label.

- verbosity:

  Integer how much output to print. 0: silent; 1: normal output; 2:
  display messages from predict() function.

- da_mode:

  String containing the type of differential abundance being seeked,
  either "lda" (local DA) or "gda" (global DA).

## Value

A vector containing a null distribution of Dawnn's model outputs for
shuffled sample labels.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_null_dist(cells = cell_object, model = nn_model, label_names =
"synth_labels", verbosity = 1, da_mode = "lda")
} # }
```
