# Identify which cells are in regions of differential abundance using Dawnn.

`run_dawnn()` is the main function used to run Dawnn. It takes a Seurat
dataset and identifies which cells are in regions of differential
abundance. Dawnn requires at least 1,001 cells.

## Usage

``` r
run_dawnn(
  cells,
  label_names,
  label_1,
  label_2,
  reduced_dim,
  n_dims = 10,
  nn_model = "~/.dawnn/dawnn_nn_model.h5",
  recalculate_graph = TRUE,
  alpha = 0.1,
  verbosity = 2,
  seed = 123,
  tf_conda_env = NULL
)
```

## Arguments

- cells:

  Seurat object containing the dataset.

- label_names:

  String containing the name of the meta.data slot in \`cells'
  containing the labels of each cell.

- label_1:

  String containing the name of one of the labels.

- label_2:

  String containing the name of the other label.

- reduced_dim:

  String containing the name of the dimensionality reduction to use.

- n_dims:

  Integer number of dimensions to use if computing graph (optional,
  default 10).

- nn_model:

  String containing the path to the model's .hdf5 file (optional,
  default "~/.dawnn/dawnn_nn_model.h5").

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

Seurat dataset \`cells' with added metadata: dawnn_scores (output of
Dawnn's model for each cell); dawnn_lfc (estimated log2-fold change in
the neighbourhood of each cell); dawnn_p_vals (p-values associated with
the hypothesis tests for whether a cell is in a region of differential
abundance; dawnn_da_verdict (Boolean output of Dawnn indicating whether
it considers a cell to be in a region of differential abundance).

## Examples

``` r
if (FALSE) { # \dontrun{
run_dawnn(cells = dataset, label_names = "condition", nn_model =
"my_model.h5", reduced_dim = "pca", n_dims = 50, recalculate_graph = FALSE,
alpha = 0.2, verbosity = 0, seed = 42, tf_conda_env = "my_tensorflow_env")
} # }
```
