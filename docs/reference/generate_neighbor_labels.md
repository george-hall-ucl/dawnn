# Generate a matrix of the labels of the 1,000 nearest neighbors of each cell.

Generate a matrix of the labels of the 1,000 nearest neighbors of each
cell.

## Usage

``` r
generate_neighbor_labels(cells, verbose, label_names, label_1)
```

## Arguments

- cells:

  Seurat object containing the dataset.

- verbose:

  Boolean verbosity.

- label_names:

  String containing the name of the meta.data slot in \`cells'
  containing the labels of each cell.

- label_1:

  String containing the name of one of the labels.

## Value

A data frame containing the labels of the 1000 nearest neighbors of each
cell.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_neighbor_labels(cell_object, verbose = TRUE, label_names =
"sample_names", label_1 = "Condition1")
} # }
```
