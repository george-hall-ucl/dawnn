library(Seurat)
sm <- suppressMessages

test_that("generate_null_distribution returns vector of correct length", {
    cells <- readRDS("../data/dawnn_test_data_1000_cells_discrete_clusters.rds")

    set.seed(123)

    model_file <- "../data/final_model_dawnn.h5"
    model <- load_model_from_python(model_file)
    out <- sm(generate_null_dist(cells, model, label_names = "label",
                                 label_1 = "Condition1",
                                 label_2 = "Condition2",
                                 verbosity = 1))
    expect_vector(out, ptype = double(), size = 8100)
})
