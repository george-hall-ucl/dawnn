library(Seurat)

test_that("determine_if_region_da_pc1 reproducible", {
    set.seed(123)
    s <- rbeta(n = 1000, shape1 = 6, shape2 = 4)
    n <- rbeta(n = 1000, shape1 = 50, shape2 = 50)
    p <- rbeta(n = 1000, shape1 = 0.7, shape2 = 0.4)
    verdict_1 <- determine_if_region_da_pc1(p_vals = p, scores = s,
                                            null_dist = n, alpha = 0.1,
                                            assume_independence = TRUE,
                                            method = "beta")
    verdict_2 <- determine_if_region_da_pc1(p_vals = p, scores = s,
                                            null_dist = n, alpha = 0.1,
                                            assume_independence = TRUE,
                                            method = "beta")
    expect_equal(verdict_1, verdict_2)
})

test_that("determine_if_region_da_pc1 returns vector", {
    set.seed(123)
    s <- rbeta(n = 1000, shape1 = 6, shape2 = 4)
    n <- rbeta(n = 1000, shape1 = 50, shape2 = 50)
    p <- rbeta(n = 1000, shape1 = 0.7, shape2 = 0.4)
    verdict <- determine_if_region_da_pc1(p_vals = p, scores = s,
                                          null_dist = n, alpha = 0.1,
                                          assume_independence = TRUE,
                                          method = "beta")
    expect_vector(verdict, ptype = logical(), size = 1000)
})
