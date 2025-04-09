test_that("Can change conda env to one containing TensorFlow", {
    res_list <- list()
    for (tf_conda_env_val in c("none_set", "tf_env")) {
        cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
        cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                               dims = 1:2, return.neighbor = TRUE)
        if (tf_conda_env_val == "none_set") {
            dawnn_out <- sm(run_dawnn(cells = cells, label_names = "label",
                                      label_1 = "Condition1", label_2 = "Condition2",
                                      reduced_dim = "pca", recalculate_graph = FALSE,
                                      alpha = 0.1, verbosity = 0))
        } else if (tf_conda_env_val == "tf_env") {
            dawnn_out <- sm(run_dawnn(cells = cells, label_names = "label",
                                      label_1 = "Condition1", label_2 = "Condition2",
                                                reduced_dim = "pca", recalculate_graph = FALSE,
                                                alpha = 0.1, verbosity = 0, tf_conda_env = "tf_env"))
        }
        res_list[[tf_conda_env_val]] <- dawnn_out
    }
    expect_equal(res_list[["none_set"]], res_list[["tf_env"]])
})

test_that("Dawnn crashes if change conda env to one without TensorFlow", {
    m <- paste("Tensorflow not installed in reticulate environment\\\\?.",
               "Please install following",
               "rstudio\\\\?.github\\\\?.io/reticulate/articles/python_packages\\\\?.html\\\\?.")

    res <- try(callr::r(function() {
        devtools::load_all("../..")
        cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
        cells <- Seurat::FindNeighbors(cells, reduction = "pca",
                                       k.param = 1001, dims = 1:2,
                                       return.neighbor = TRUE)
        dawnn::run_dawnn(cells = cells, label_names = "label",
                         label_1 = "Condition1", label_2 = "Condition2",
                         reduced_dim = "pca", recalculate_graph = FALSE,
                         alpha = 0.1, verbosity = 0, tf_conda_env = "base")}))

    expect_match(res[[1]], m)
    expect_s3_class(res, "try-error")
})
