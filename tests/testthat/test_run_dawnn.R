# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

test_that("run_dawnn reproducible recalculate_graph = FALSE", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                           dims = 1:2, return.neighbor = TRUE)

    dawnn_out_1 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", recalculate_graph = FALSE,
                                alpha = 0.1, verbosity = 0))
    dawnn_out_2 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", recalculate_graph = FALSE,
                                alpha = 0.1, verbosity = 0))

    expect_equal(dawnn_out_1, dawnn_out_2)
})

test_that("run_dawnn reproducible recalculate_graph = TRUE", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                           dims = 1:2, return.neighbor = TRUE)

    dawnn_out_1 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", n_dims = 2,
                                recalculate_graph = TRUE, alpha = 0.1,
                                verbosity = 0))

    dawnn_out_2 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", n_dims = 2,
                                recalculate_graph = TRUE, alpha = 0.1,
                                verbosity = 0))

    expect_equal(dawnn_out_1, dawnn_out_2)
})

test_that("run_dawnn returns Seurat", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                           dims = 1:2, return.neighbor = TRUE)

    dawnn_out <- sm(run_dawnn(cells = cells, label_names = "label",
                              label_1 = "Condition1", label_2 = "Condition2",
                              reduced_dim = "pca", recalculate_graph = FALSE,
                              alpha = 0.1, verbosity = 0))
    expect_s4_class(dawnn_out, "Seurat")
})

test_that("run_dawnn fails if too few cells", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                           dims = 1:2, return.neighbor = TRUE)

    expect_error(sm(run_dawnn(cells = cells[, 1:1000], label_names = "label",
                              label_1 = "Condition1", label_2 = "Condition2",
                              reduced_dim = "pca", recalculate_graph = FALSE,
                              alpha = 0.1, verbosity = 0)),
                 "Dawnn requires at least 1001 cells. Your dataset contains 1000.")
})
