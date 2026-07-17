# Sanity check input parameters

\`param_check()' verifies that the parameters passed to run_dawnn() are
sane.

## Usage

``` r
param_check(
  cells,
  label_names,
  label_1,
  label_2,
  reduced_dim,
  recalculate_graph
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

- recalculate_graph:

  Boolean whether to recalculate the KNN graph. If FALSE, then the one
  stored in the `cells` object will be used (optional, default = TRUE).

## Value

TRUE if all parameters sane, otherwise stop execution with error
message.

## Examples

``` r
if (FALSE) { # \dontrun{
param_check(cells, label_names, label_1, label_2, reduced_dim,
recalculate_graph)
} # }
```
