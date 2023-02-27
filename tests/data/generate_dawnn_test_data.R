library(Seurat)
set.seed(123)
cells <- readRDS("dawnn/tests/data/cells_sim_discerete_clusters_gex_seed_1.rds")
label_idxs <- as.numeric(read.csv("dawnn/tests/data/benchmark_dataset_sim_discrete_clusters.csv", header = FALSE)[1, 4:2703]) + 1
labels <- c("Condition2", "Condition1")[label_idxs]
cells$label <- labels
cells <- FindNeighbors(cells, dims = 1:50, return.neighbor = TRUE, k.param = 1000+1, reduction = "pca")
saveRDS(cells, "dawnn/tests/data/dawnn_test_data_1000_cells_discrete_clusters.rds")
