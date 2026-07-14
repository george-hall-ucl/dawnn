# Copyright (C) 2023-2025 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

test_that("Incorrect model_path leads to error", {
              results <- try(sep_r(function() {
                                       reticulate::use_condaenv("tf_env")
                                       dawnn:::load_model_from_python("this_model_does_not_exist.h5")},
                                       print_stdout = FALSE))
              error_msg <- strsplit((results[[1]]), "\n")[[1]][3]
              expect_s3_class(results, "try-error")
              expect_equal(error_msg, "! No model available at this_model_does_not_exist.h5")
})

test_that("Loaded model has correct structure and weights", {
              # Ideally, we would be able to compute a checksum of all weights
              # in each tensor, but I can't find a way of doing this. For now,
              # summing them seems sufficient.
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

              actual_model_summary_sum <- sep_r(function() {
                reticulate::use_condaenv("tf_env")
                model <- dawnn:::load_model_from_python("~/.dawnn/dawnn_nn_model.h5")
                actual_model_summary <- keras:::format.keras.engine.training.Model(model)
                actual_model_sum <- sum(unlist(lapply(model$weights,
                                                      function(x) {
                                                          as.numeric(sum(x))
                                                      })))
                # Only look to four decimal places, as in goal_model_sum
                actual_model_sum <- round(actual_model_sum, digits = 4)
                return(paste(actual_model_summary, actual_model_sum))
              })
              expect_equal(actual_model_summary_sum, goal_model_summary_sum)
})
