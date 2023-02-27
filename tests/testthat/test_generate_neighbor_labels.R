library(Seurat)

test_that("generate_neighbor_labels check correctness and class", {
    cells <- readRDS("../data/five_cell_seurat.rds")
    # cells$label is c("Condition1", "Condition2", "Condition1",
    #                  "Condition2", "Condition1")

    set.seed(123)
    mat <- cbind(1:5, t(apply(t(matrix(rep(1:5, 5), ncol = 5)), 1, sample)))
    #      [,1] [,2] [,3] [,4] [,5] [,6]
    # [1,]    1    3    2    5    4    1
    # [2,]    2    3    1    2    5    4
    # [3,]    3    2    3    1    4    5
    # [4,]    4    1    4    5    3    2
    # [5,]    5    3    4    2    1    5 
    cells@neighbors$RNA.nn@nn.idx <- mat

    actual <- generate_neighbor_labels(cells, label_names = "label",
                                       label_1 = "Condition1", verbose = FALSE)
    # Should be:
    #      [,1] [,2] [,3] [,4] [,5]
    # [1,]    1    0    1    0    1
    # [2,]    1    1    0    1    0
    # [3,]    0    1    1    0    1
    # [4,]    1    0    1    1    0
    # [5,]    1    0    0    1    1

    expected <- t(matrix(c(1, 0, 1, 0, 1,
                           1, 1, 0, 1, 0,
                           0, 1, 1, 0, 1,
                           1, 0, 1, 1, 0,
                           1, 0, 0, 1, 1),
                         ncol = 5))

    expect_equal(actual, expected)
    expect_equal(class(actual), c("matrix", "array"))
})
