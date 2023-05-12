# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

test_that("beta_method_of_moments fitted parameters", {
              set.seed(123)
              data <- rbeta(n = 1000000, shape1 = 5, shape2 = 2)
              params <- beta_method_of_moments(data)
              expect_equal(params$alpha, 4.997687, tolerance = 0.001)
              expect_equal(params$beta, 1.998383, tolerance = 0.001)
})

test_that("beta_method_of_moments output format", {
              set.seed(123)
              data <- rbeta(n = 1000000, shape1 = 5, shape2 = 2)
              params <- beta_method_of_moments(data)
              expect_type(params, "list")
              expect_length(params, 2)
})
