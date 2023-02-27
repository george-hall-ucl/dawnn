library(Seurat)

test_that("generate_null_distribution returns vector of correct length", {
    cells <- readRDS("../data/cells_sim_discerete_clusters_gex_seed_1.rds")
    label_idxs <- as.numeric(read.csv("../data/benchmark_dataset_sim_discrete_clusters.csv", header = FALSE)[1, 4:2703]) + 1
    labels <- c("Condition2", "Condition1")[label_idxs]
    cells$label <- labels
    cells <- FindNeighbors(cells, dims = 1:50, return.neighbor = TRUE, k.param = 1000+1, reduction = "pca")

    set.seed(123)

    model_file <- "../data/final_model_dawnn.h5"
    model <- load_model_from_python(model_file)
    out <- generate_null_dist(cells, model, label_names = "label",
                              enforce_05 = TRUE, verbosity = 1)
    expect_vector(out, ptype = double(), size = 8100)
})
