<p align="center">
  <img src="man/figures/dawnn_logo.png" width="150">
  <br><br>
  Dawnn is a method to detect differential abundance in a single-cell
  transcriptomic dataset.
</p>

### Installation

Dawnn is currently only available from Github:

```
devtools::install_github("george-hall-ucl/dawnn")
```

### How to use

In the following, we assume that `cells` is a Seurat single-cell dataset with
\>1000 cells and a stored PCA dimensionality reduction. We also assume that
each cell has one of two labels (`Condition1` or `Condition2`), stored in a
metadata slot called "labels".

Dawnn can be run with:

```{r}
library(dawnn)

# Step 1: Download Dawnn's model:
# Only run once per machine.
# By default, model stored at ~/.dawnn/dawnn_nn_model.h5 (can be changed with
#     model_file_path parameter)
download_model()

# Step 2: Run Dawnn:
cells <- run_dawnn(cells, label_names = "label", label_1 = "Condition1",
                   label_2 = "Condition2", reduced_dim = "pca")

# The above example only specified the required parameters. We can specify more
# parameters as follows:
cells <- run_dawnn(cells = cells, label_names = "label",
                   label_1 = "Condition1", label_2 = "Condition2",
                   reduced_dim = "pca", n_dims = 20,
                   nn_model = "~/Documents/another_nn_model.h5,
                   recalculate_graph = FALSE, alpha = 0.025,
                   verbosity = 0, seed = 42)
```

After `run_dawnn()`, the object `cells` has additional metadata for each cell:

| Dawnn output             | Description                                                                                   |
|--------------------------|-----------------------------------------------------------------------------------------------|
| `cells$dawnn_scores`     | Output of Dawnn's model.                                                                      |
| `cells$dawnn_lfc`        | Estimated log2-fold change in its neighbourhood.                                              |
| `cells$dawnn_p_vals`     | P-value associated with the hypothesis test that it is in a region of differential abundance. |
| `cells$dawnn_da_verdict` | Boolean output of Dawnn for whether it is in a region of differential abundance.              |
