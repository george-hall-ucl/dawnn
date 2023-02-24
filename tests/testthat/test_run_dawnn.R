library(Seurat)

# This test currently passes, however:
# * It crashes if I set recalculate_graph to TRUE
test_that("run_dawnn reproducible recalculate_graph = FALSE", {
    cells <- readRDS("../data/cells_sim_discerete_clusters_gex_seed_1.rds")
    label_idxs <- as.numeric(read.csv("../data/benchmark_dataset_sim_discrete_clusters.csv", header = FALSE)[1, 4:2703]) + 1
    labels <- c("Condition2", "Condition1")[label_idxs]
    cells$label <- labels
    cells <- FindNeighbors(cells, dims = 1:50, return.neighbor = TRUE, k.param = 1000+1, reduction = "pca")

    model_file <- "../data/final_model_dawnn.h5"
    dawnn_out_1 <- run_dawnn(cells = cells, label_names = "label",
                             nn_model = model_file, reduced_dim = "pca",
                             recalculate_graph = FALSE, two_sided = TRUE,
                             alpha = 0.1, verbosity = 0)
    dawnn_out_2 <- run_dawnn(cells = cells, label_names = "label",
                             nn_model = model_file, reduced_dim = "pca",
                             recalculate_graph = FALSE, two_sided = TRUE,
                             alpha = 0.1, verbosity = 0)

    expect_equal(dawnn_out_1, dawnn_out_2)
})

test_that("run_dawnn reproducible recalculate_graph = TRUE", {
    cells <- readRDS("../data/cells_sim_discerete_clusters_gex_seed_1.rds")
    label_idxs <- as.numeric(read.csv("../data/benchmark_dataset_sim_discrete_clusters.csv", header = FALSE)[1, 4:2703]) + 1
    labels <- c("Condition2", "Condition1")[label_idxs]
    cells$label <- labels

    model_file <- "../data/final_model_dawnn.h5"
    dawnn_out_1 <- run_dawnn(cells = cells, label_names = "label",
                             nn_model = model_file, reduced_dim = "pca",
                             recalculate_graph = TRUE, two_sided = TRUE,
                             alpha = 0.1, verbosity = 0)

    dawnn_out_2 <- run_dawnn(cells = cells, label_names = "label",
                             nn_model = model_file, reduced_dim = "pca",
                             recalculate_graph = TRUE, two_sided = TRUE,
                             alpha = 0.1, verbosity = 0)

    expect_equal(dawnn_out_1, dawnn_out_2)
})

test_that("run_dawnn returns Seurat", {
    cells <- readRDS("../data/cells_sim_discerete_clusters_gex_seed_1.rds")
    label_idxs <- as.numeric(read.csv("../data/benchmark_dataset_sim_discrete_clusters.csv", header = FALSE)[1, 4:2703]) + 1
    labels <- c("Condition2", "Condition1")[label_idxs]
    cells$label <- labels

    model_file <- "../data/final_model_dawnn.h5"
    dawnn_out <- run_dawnn(cells = cells, label_names = "label",
                           nn_model = model_file, reduced_dim = "pca",
                           recalculate_graph = TRUE, two_sided = TRUE,
                           alpha = 0.1, verbosity = 0)
    expect_s4_class(dawnn_out, "Seurat")
})
