library(Seurat)

test_that("param_check error if label_1 same as label_2", {
    cells <- readRDS("../data/five_cell_seurat_2gene.rds")

    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition1",
                             reduced_dim = "foo", recalculate_graph = FALSE),
                 "label_1 and label_2 must not be the same.")
})

test_that("param_check error if not two unique labels", {
    cells <- readRDS("../data/five_cell_seurat_2gene.rds")

    cells$label <- rep("Condition1", ncol(cells))
    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition2",
                             reduced_dim = "foo", recalculate_graph = FALSE),
                 "There must be exactly two distinct labels.")

    cells$label <- c("Condition1", "Condition2", "Condition3", "Condition1",
                     "Condition2")
    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition2",
                             reduced_dim = "foo", recalculate_graph = FALSE),
                 "There must be exactly two distinct labels.")
})

test_that("param_check error if both labels do not appear", {
    cells <- readRDS("../data/five_cell_seurat_2gene.rds")

    # Only label_1 assigned to cells
    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition3",
                             reduced_dim = "foo", recalculate_graph = FALSE),
                 "Both label_1 and label_2 must be assigned to at least one cell.")

    # Only label_2 assigned to cells
    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition3", label_2 = "Condition2",
                             reduced_dim = "foo", recalculate_graph = FALSE),
                 "Both label_1 and label_2 must be assigned to at least one cell.")

    # Neither label_1 nor label_2 assigned to any cell
    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition3", label_2 = "Condition4",
                             reduced_dim = "foo", recalculate_graph = FALSE),
                 "Both label_1 and label_2 must be assigned to at least one cell.")
})

test_that("param_check error if reduced_dim does not exist", {
    cells <- readRDS("../data/five_cell_seurat_2gene.rds")
    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition2",
                             reduced_dim = "foo", recalculate_graph = FALSE),
                 "No dimensionality reduction: foo")
})

test_that("param_check error if no graph with recalculate_graph = FALSE", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    cells@meta.data$label <- c("Condition1", "Condition2")
    expect_error(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition2",
                             reduced_dim = "pca", recalculate_graph = FALSE),
                 paste("No K-nearest-neighbor graph but recalculate_graph is",
                       "FALSE. Set to TRUE or run Seurat::FindNeighbors().")
)
})

test_that("param_check no error if no problem", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    cells@meta.data$label <- c("Condition1", "Condition2")
    expect_equal(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition2",
                             reduced_dim = "pca", recalculate_graph = TRUE),
                 TRUE)

    cells <- readRDS("../data/five_cell_seurat_2gene.rds")
    expect_equal(param_check(cells, label_names = "label",
                             label_1 = "Condition1", label_2 = "Condition2",
                             reduced_dim = "pca", recalculate_graph = FALSE),
                 TRUE)
})
