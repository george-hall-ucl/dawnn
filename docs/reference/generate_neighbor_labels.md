# Generate a matrix of the labels of the 1,000 nearest neighbors of each cell.

Generate a matrix of the labels of the 1,000 nearest neighbors of each
cell.

## Usage

``` r
generate_neighbor_labels(cells, verbose, label_names, label_pos_lfc)
```

## Arguments

- cells:

  Seurat object containing the dataset.

- verbose:

  Boolean verbosity.

- label_names:

  String containing the name of the meta.data slot in `cells` containing
  the labels of each cell.

- label_pos_lfc:

  String containing the name of the label associated with positive
  log-fold change.

## Value

A data frame containing the labels of the 1000 nearest neighbors of each
cell.
