test_that("Can change conda env to one containing TensorFlow", {
    result <- sep_r(function() {
        devtools::load_all("../..")
        cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
        cells <- Seurat::FindNeighbors(cells, reduction = "pca", k.param = 1001,
                                       dims = 1:2, return.neighbor = TRUE)
        dawnn::run_dawnn(cells = cells, label_names = "label", label_1 = "Condition1",
                         label_2 = "Condition2", reduced_dim = "pca",
                         recalculate_graph = FALSE, alpha = 0.1, verbosity = 0,
                         tf_conda_env = "tf_env")
    })
    expect_equal(sum(result$dawnn_p_vals), 269.095362)
})


test_that("Crashes if no conda env specified and correct packages not installed in base", {
    result <- try(sep_r(function() {
        devtools::load_all("../..")
        cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
        cells <- Seurat::FindNeighbors(cells, reduction = "pca", k.param = 1001,
                                       dims = 1:2, return.neighbor = TRUE)
        dawnn::run_dawnn(cells = cells, label_names = "label", label_1 = "Condition1",
                         label_2 = "Condition2", reduced_dim = "pca",
                         recalculate_graph = FALSE, alpha = 0.1, verbosity = 0)
    }, print_stdout = FALSE))
    expect_s3_class(result, "try-error")
})

test_that("Dawnn crashes if change conda env to one without TensorFlow", {
    result <- try(sep_r(function() {
        devtools::load_all("../..")
        cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
        cells <- Seurat::FindNeighbors(cells, reduction = "pca", k.param = 1001,
                                       dims = 1:2, return.neighbor = TRUE)
        dawnn::run_dawnn(cells = cells, label_names = "label", label_1 = "Condition1",
                         label_2 = "Condition2", reduced_dim = "pca",
                         recalculate_graph = FALSE, alpha = 0.1, verbosity = 0,
                         tf_conda_env = "base")
    }, print_stdout = FALSE))
    expect_s3_class(result, "try-error")
})
