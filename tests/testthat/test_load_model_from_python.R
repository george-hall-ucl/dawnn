# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

test_that("Incorrect model_path leads to error", {
              expect_error(load_model_from_python("this_model_does_not_exist.h5"),
                           "No model available at this_model_does_not_exist.h5")
})

test_that("Loaded model has the correct class", {
              model <- load_model_from_python("~/.dawnn/dawnn_nn_model.h5")
              model_class <- class(model)[1:4]
              goal_class_values <- c("keras.engine.sequential.Sequential",
                                     "keras.engine.functional.Functional",
                                     "keras.engine.training.Model",
                                     "keras.engine.base_layer.Layer")
              expect_equal(model_class, goal_class_values)
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
