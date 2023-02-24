test_that("download_model downloads a model", {
              # # Hacky setup for now as the model is hosted on a private repo
              # system("sh ../../../curling.sh > ../../../curling_out.txt")
              # href <- strsplit(readLines("../../../curling_out.txt"), " ")[[1]][14]
              # href <- substr(href, 2, nchar(href) - 2)
              # model_path <- download_model(model_url = href,
              #                              model_file_path = "/tmp/dawnn_nn_model.h5")
              # print(model_path)
              # # same as in test_load_model_from_python.R
              # model <- load_model_from_python(model_path)
              # model_class <- class(model)
              # goal_class_values <- c("keras.engine.sequential.Sequential",
              #                        "keras.engine.functional.Functional",
              #                        "keras.engine.training.Model",
              #                        "keras.engine.base_layer.Layer",
              #                        "tensorflow.python.module.module.Module",
              #                        "tensorflow.python.training.tracking.autotrackable.AutoTrackable",
              #                        "tensorflow.python.training.tracking.base.Trackable",
              #                        "keras.utils.version_utils.LayerVersionSelector",
              #                        "keras.utils.version_utils.ModelVersionSelector",
              #                        "python.builtin.object")
              # expect_equal(model_class, goal_class_values)

})
