# Copyright (C) 2023-2025 University College London
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

test_that("run_dawnn different results if different da_mode", {
    # Note that the labels are distributed as follows:
    #   Condition1 Condition2
    #          595        605
    # run_dawnn with da_mode set to "ada" should therefore return a different
    # object to if da_mode is "pda" (since the p-values should be different).
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                           dims = 1:2, return.neighbor = TRUE)

    dawnn_out_1 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", recalculate_graph = FALSE,
                                alpha = 0.1, verbosity = 0, da_mode = "ada"))
    dawnn_out_2 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", recalculate_graph = FALSE,
                                alpha = 0.1, verbosity = 0, da_mode = "pda"))

    expect_failure(expect_equal(dawnn_out_1, dawnn_out_2))
})

test_that("run_dawnn same results if different da_mode but exactly even labels", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    # Make exacetly 600 labels with each condition:
    cells$label[which(cells$label == "Condition2")[1:5]] <- "Condition1"

    cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                           dims = 1:2, return.neighbor = TRUE)

    dawnn_out_1 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", recalculate_graph = FALSE,
                                alpha = 0.1, verbosity = 0, da_mode = "ada"))
    dawnn_out_2 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                reduced_dim = "pca", recalculate_graph = FALSE,
                                alpha = 0.1, verbosity = 0, da_mode = "pda"))

    expect_equal(dawnn_out_1, dawnn_out_2)
})
