# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

test_that("Incorrect model_path leads to error", {
              expect_error(load_model_from_python("this_model_does_not_exist.h5"),
                           "No model available at this_model_does_not_exist.h5")
})

test_that("Loaded model has correct structure and weights", {
              # Ideally, we would be able to compute a checksum of all weights
              # in each tensor, but I can't find a way of doing this. For now,
              # summing them seems sufficient.
              model <- load_model_from_python("~/.dawnn/dawnn_nn_model.h5")
              goal_model_summary <- "Model: \"sequential\"
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━┓
┃ Layer (type)                      ┃ Output Shape             ┃       Param # ┃
┡━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━┩
│ dense (Dense)                     │ (None, 1000)             │     1,001,000 │
├───────────────────────────────────┼──────────────────────────┼───────────────┤
│ dense_1 (Dense)                   │ (None, 2000)             │     2,002,000 │
├───────────────────────────────────┼──────────────────────────┼───────────────┤
│ dense_2 (Dense)                   │ (None, 4000)             │     8,004,000 │
├───────────────────────────────────┼──────────────────────────┼───────────────┤
│ dense_3 (Dense)                   │ (None, 2000)             │     8,002,000 │
├───────────────────────────────────┼──────────────────────────┼───────────────┤
│ dense_4 (Dense)                   │ (None, 1000)             │     2,001,000 │
├───────────────────────────────────┼──────────────────────────┼───────────────┤
│ dense_5 (Dense)                   │ (None, 250)              │       250,250 │
├───────────────────────────────────┼──────────────────────────┼───────────────┤
│ dense_6 (Dense)                   │ (None, 10)               │         2,510 │
├───────────────────────────────────┼──────────────────────────┼───────────────┤
│ dense_7 (Dense)                   │ (None, 1)                │            11 │
└───────────────────────────────────┴──────────────────────────┴───────────────┘
 Total params: 21,262,771 (81.11 MB)
 Trainable params: 21,262,771 (81.11 MB)
 Non-trainable params: 0 (0.00 B)"
              goal_model_sum <- -415.2789
              goal_model_summary_sum <- paste(goal_model_summary,
                                              goal_model_sum)

              actual_model_summary <- keras:::format.keras.engine.training.Model(model)
              actual_model_sum <- sum(unlist(lapply(model$weights,
                                                    function(x) {
                                                        as.numeric(sum(x))
                                                    })))
              # Only look to four decimal places, as in goal_model_sum
              actual_model_sum <- round(actual_model_sum, digits = 4)
              actual_model_summary_sum <- paste(actual_model_summary,
                                                actual_model_sum)
              expect_equal(actual_model_summary_sum, goal_model_summary_sum)
})

test_that("No Tensorflow module leads to crash", {
    m <- paste("Tensorflow not installed in reticulate environment\\\\?.",
               "Please install following",
               "rstudio\\\\?.github\\\\?.io/reticulate/articles/python_packages\\\\?.html\\\\?.")

    # Use non-existant Python environment to simulate lack of Tensorflow
    # module.  callr is needed here since otherwise the loaded Tensorflow
    # library from earlier tests means that the module is still findable even
    # when RETICULATE_PYTHON_ENV pointed away from it.
    res <- try(callr::r(function() {
        withr::local_envvar(c("RETICULATE_PYTHON_ENV" = "no_tensorflow_env"))
        dawnn:::load_model_from_python("~/.dawnn/dawnn_nn_model.h5")
    }))

    expect_match(res[[1]], m)
    expect_s3_class(res, "try-error")
})
