library(Seurat)
sm <- suppressMessages

# This test currently passes, however:
# * It crashes if I set recalculate_graph to TRUE
test_that("run_dawnn reproducible recalculate_graph = FALSE", {
    cells <- readRDS("../data/dawnn_test_data_1000_cells_discrete_clusters.rds")

    model_file <- "~/.dawnn/dawnn_nn_model.h5"
    dawnn_out_1 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                nn_model = model_file, reduced_dim = "pca",
                                recalculate_graph = FALSE, alpha = 0.1,
                                verbosity = 0))
    dawnn_out_2 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                nn_model = model_file, reduced_dim = "pca",
                                recalculate_graph = FALSE, alpha = 0.1,
                                verbosity = 0))

    expect_equal(dawnn_out_1, dawnn_out_2)
})

test_that("run_dawnn reproducible recalculate_graph = TRUE", {
    cells <- readRDS("../data/dawnn_test_data_1000_cells_discrete_clusters.rds")

    model_file <- "~/.dawnn/dawnn_nn_model.h5"
    dawnn_out_1 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                nn_model = model_file, reduced_dim = "pca",
                                recalculate_graph = TRUE, alpha = 0.1,
                                verbosity = 0))

    dawnn_out_2 <- sm(run_dawnn(cells = cells, label_names = "label",
                                label_1 = "Condition1", label_2 = "Condition2",
                                nn_model = model_file, reduced_dim = "pca",
                                recalculate_graph = TRUE, alpha = 0.1,
                                verbosity = 0))

    expect_equal(dawnn_out_1, dawnn_out_2)
})

test_that("run_dawnn returns Seurat", {
    cells <- readRDS("../data/dawnn_test_data_1000_cells_discrete_clusters.rds")

    model_file <- "~/.dawnn/dawnn_nn_model.h5"
    dawnn_out <- sm(run_dawnn(cells = cells, label_names = "label",
                              label_1 = "Condition1", label_2 = "Condition2",
                              nn_model = model_file, reduced_dim = "pca",
                              recalculate_graph = TRUE, alpha = 0.1,
                              verbosity = 0))
    expect_s4_class(dawnn_out, "Seurat")
})
