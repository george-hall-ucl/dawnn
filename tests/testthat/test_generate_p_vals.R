# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

# generate_p_vals() as it was before being vectorised in 2.1.0, moved here so that we
# can verify that vectorising it changed no p-values.
generate_p_vals_reference <- function(scores, null_dist) {
    null_dist_est_params <- dawnn:::beta_method_of_moments(null_dist)
    null_alpha <- null_dist_est_params$alpha
    null_beta <- null_dist_est_params$beta
    null_mode <- (null_alpha - 1) / (null_alpha + null_beta - 2)

    p_vals <- c()
    for (score in scores) {
        if (score <= null_mode) {
            p_vals <- c(p_vals, pbeta(score, null_alpha, null_beta))
        } else {
            p_vals <- c(p_vals, 1 - pbeta(score, null_alpha, null_beta))
        }
    }

    return(p_vals)
}


test_that("generate_p_vals matches the pre-vectorisation implementation", {
    set.seed(123)
    scores <- rbeta(n = 1000, shape1 = 6, shape2 = 4)
    null_dists <- list(
        rbeta(n = 1000, shape1 = 50, shape2 = 50),
        rbeta(n = 1000, shape1 = 20, shape2 = 60),
        rbeta(n = 1000, shape1 = 60, shape2 = 20)
    )

    for (null_dist in null_dists) {
        expect_identical(
            generate_p_vals(scores, null_dist),
            generate_p_vals_reference(scores, null_dist)
        )
    }
})


test_that("generate_p_vals matches the old version given the a matrix", {
    # Since predict() returns an n x 1 matrix rather than a vector, we should check that
    # both functions give identical answers in this case too (rather than just the
    # vector case covered in the other test).
    set.seed(123)
    scores <- matrix(rbeta(n = 1000, shape1 = 6, shape2 = 4), ncol = 1)
    null_dist <- rbeta(n = 1000, shape1 = 50, shape2 = 50)

    expect_identical(
        generate_p_vals(scores, null_dist),
        generate_p_vals_reference(scores, null_dist)
    )
})


test_that("generate_p_vals reproducible", {
    set.seed(123)
    s <- rbeta(n = 1000, shape1 = 6, shape2 = 4)
    n <- rbeta(n = 1000, shape1 = 50, shape2 = 50)
    p_vals_1 <- generate_p_vals(scores = s, null_dist = n)
    p_vals_2 <- generate_p_vals(scores = s, null_dist = n)
    expect_equal(p_vals_1, p_vals_2)
})

test_that("generate_p_vals returns vector", {
    set.seed(123)
    s <- rbeta(n = 1000, shape1 = 6, shape2 = 4)
    n <- rbeta(n = 1000, shape1 = 50, shape2 = 50)
    p_vals <- generate_p_vals(scores = s, null_dist = n)
    expect_vector(p_vals, ptype = double(), size = 1000)
})
