# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

test_that("determine_if_region_da reproducible", {
    set.seed(123)
    p <- rbeta(n = 1000, shape1 = 0.7, shape2 = 0.4)
    verdict_1 <- determine_if_region_da(p_vals = p, alpha = 0.1)
    verdict_2 <- determine_if_region_da(p_vals = p, alpha = 0.1)
    expect_equal(verdict_1, verdict_2)
})

test_that("determine_if_region_da returns vector", {
    set.seed(123)
    p <- rbeta(n = 1000, shape1 = 0.7, shape2 = 0.4)
    verdict <- determine_if_region_da(p_vals = p, alpha = 0.1)
    expect_vector(verdict, ptype = logical(), size = 1000)
})
