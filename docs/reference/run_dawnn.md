# Identify which cells are in regions of differential abundance using Dawnn.

`run_dawnn()` is the main function used to run Dawnn. It takes a Seurat
dataset and identifies which cells are in regions of differential
abundance. Dawnn requires at least 1,001 cells.

## Usage

``` r
run_dawnn(
  cells,
  label_names,
  label_pos_lfc,
  reduced_dim,
  n_dims = 10,
  nn_model = dawnn_default_model_file(),
  recalculate_graph = TRUE,
  alpha = 0.1,
  verbosity = 1,
  seed = 123,
  tf_conda_env = NULL
)
```

## Arguments

- cells:

  Seurat object containing the dataset.

- label_names:

  String containing the name of the meta.data slot in `cells` containing
  the labels of each cell.

- label_pos_lfc:

  String containing the name of the label associated with positive
  log-fold change.

- reduced_dim:

  String containing the name of the dimensionality reduction to use.

- n_dims:

  Integer number of dimensions to use if computing graph (optional,
  default 10).

- nn_model:

  String containing the path to the model's .hdf5 file (optional,
  defaults to the location used by
  [`download_model()`](https://george-hall-ucl.github.io/dawnn/reference/download_model.md)).

- recalculate_graph:

  Boolean whether to recalculate the KNN graph. If FALSE, then the one
  stored in the `cells` object will be used (optional, default = TRUE).

- alpha:

  Numeric target false discovery rate supplied to the
  Benjamini–Yekutieli procedure (optional, default 0.1, i.e. 10%).

- verbosity:

  Integer how much output to print. 0: silent; 1: normal output; 2:
  display messages from predict() function.

- seed:

  Integer random seed (optional, default 123).

- tf_conda_env:

  Conda environment with TensorFlow installed, useful if it is
  unavailable in the current environment (optional, default NULL).

## Value

Seurat dataset `cells` with added metadata: `dawnn_scores` (output of
Dawnn's model for each cell); `dawnn_lfc` (estimated log2-fold change in
the neighbourhood of each cell); `dawnn_p_vals_lda` and
`dawnn_p_vals_gda` (p-values associated with the hypothesis tests for
whether a cell is in a region of local or global differential abundance,
respectively); `dawnn_lda_verdict` and `dawnn_gda_verdict` (Boolean
output of Dawnn indicating whether it considers a cell to be in a region
of local or global differential abundance, respectively).

## Examples

``` r
if (FALSE) { # \dontrun{
# Only required options
run_dawnn(
    cells = dataset, label_names = "condition", label_pos_lfc = "Condition_1",
    nn_model = "my_model.h5", reduced_dim = "pca"
)
# All options
run_dawnn(
    cells = dataset, label_names = "condition", label_pos_lfc = "Condition_1",
    nn_model = "my_model.h5", reduced_dim = "pca", n_dims = 50,
    recalculate_graph = FALSE, alpha = 0.2, verbosity = 0, seed = 42,
    tf_conda_env = "my_tensorflow_env"
)
} # }
```
