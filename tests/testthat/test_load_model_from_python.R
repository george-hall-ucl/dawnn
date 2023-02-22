test_that("Incorrect model_path leads to error", {
              expect_error(load_model_from_python("this_model_does_not_exist.h5"),
                           "No model available at this_model_does_not_exist.h5")
})

test_that("Loaded model has the correct class", {
              model <- load_model_from_python("~/Documents/Analysis/own_da_method/final_model_dawnn.h5")
              model_class <- class(model)
              goal_class_values <- c("keras.engine.sequential.Sequential",
                                     "keras.engine.functional.Functional",
                                     "keras.engine.training.Model",
                                     "keras.engine.base_layer.Layer",
                                     "tensorflow.python.module.module.Module",
                                     "tensorflow.python.training.tracking.autotrackable.AutoTrackable",
                                     "tensorflow.python.training.tracking.base.Trackable",
                                     "keras.utils.version_utils.LayerVersionSelector",
                                     "keras.utils.version_utils.ModelVersionSelector",
                                     "python.builtin.object")
              expect_equal(model_class, goal_class_values)
})
