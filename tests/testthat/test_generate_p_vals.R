library(Seurat)

test_that("generate_p_vals reproducible", {
    set.seed(123)
    s <- rbeta(n = 1000, shape1 = 6, shape2 = 4)
    n <- rbeta(n = 1000, shape1 = 50, shape2 = 50)
    p_vals_1 <- generate_p_vals(scores = s, null_dist = n,
                                two_sided = TRUE)
    p_vals_2 <- generate_p_vals(scores = s, null_dist = n,
                                two_sided = TRUE)
    expect_equal(p_vals_1, p_vals_2)

    p_vals_1 <- generate_p_vals(scores = s, null_dist = n,
                                two_sided = FALSE)
    p_vals_2 <- generate_p_vals(scores = s, null_dist = n,
                                two_sided = FALSE)
    expect_equal(p_vals_1, p_vals_2)
})

test_that("generate_p_vals returns vector", {
    set.seed(123)
    s <- rbeta(n = 1000, shape1 = 6, shape2 = 4)
    n <- rbeta(n = 1000, shape1 = 50, shape2 = 50)
    p_vals <- generate_p_vals(scores = s, null_dist = n,
                              two_sided = TRUE)
    expect_vector(p_vals, ptype = double(), size = 1000)

    p_vals <- generate_p_vals(scores = s, null_dist = n,
                              two_sided = FALSE)
    expect_vector(p_vals, ptype = double(), size = 1000)
})
