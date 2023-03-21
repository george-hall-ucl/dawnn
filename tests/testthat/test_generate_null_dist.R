library(Seurat)
sm <- suppressMessages

test_that("generate_null_distribution returns vector of correct length", {
    cells <- readRDS("../data/dawnn_test_data_1200_cells_discrete_clusters_1gene_2pc.rds")
    set.seed(123)
    cells <- FindNeighbors(cells, reduction = "pca", k.param = 1001,
                           dims = 1:2, return.neighbor = TRUE)


    model_file <- "~/.dawnn/dawnn_nn_model.h5"
    model <- load_model_from_python(model_file)
    out <- sm(generate_null_dist(cells, model, label_names = "label",
                                 label_1 = "Condition1",
                                 label_2 = "Condition2",
                                 verbosity = 1))
    expect_vector(out, ptype = double(), size = 3600)
})
