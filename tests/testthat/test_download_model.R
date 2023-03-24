test_that("download_model functions with no arguments", {
              local_envvar(c("HOME" = paste0(devtools::package_file(), "/tests/.tmp_home/.can_write")))
              expected_model_path <- paste0(Sys.getenv("HOME"), "/.dawnn/dawnn_nn_model.h5")
              expected_msg <- paste("Model was downloaded to:",
                                    normalizePath(expected_model_path, mustWork = FALSE))
              expect_message(download_model(), expected_msg)

              # Delete test .dawnn directory
              unlink(dirname(expected_model_path), recursive = TRUE)
})


test_that("download_model saves model in correct location", {
              local_envvar(c("HOME" = paste0(devtools::package_file(), "/tests/.tmp_home/.can_write")))
              desired_model_path <- paste0(Sys.getenv("HOME"), "/.dawnn/my_path.h5")
              expected_msg <- paste("Model was downloaded to:",
                                    normalizePath(desired_model_path, mustWork = FALSE))
              expect_message(download_model(model_file_path = desired_model_path), expected_msg)
              expect_equal(file.exists(desired_model_path), TRUE)

              # Delete test .dawnn directory
              unlink(dirname(desired_model_path), recursive = TRUE)
})


test_that("download_model stops if cannot create .dawnn", {
              local_envvar(c("HOME" = paste0(devtools::package_file(), "/tests/.tmp_home/.cannot_write")))
              expect_error(suppressWarnings(download_model()),
                           "Not downloading as cannot create ~/.dawnn directory")
})


test_that("download_model warns if downloaded file is smaller than expected", {
              local_envvar(c("HOME" = paste0(devtools::package_file(), "/tests/.tmp_home/.can_write")))
              expect_warning(download_model(model_url = "example.com"),
                             "Downloaded model file is different to expected size: wrong file?")

              # Delete test .dawnn directory
              unlink(paste0(Sys.getenv("HOME"), "/.dawnn"), recursive = TRUE)
})


test_that("download_model stops if URL is faulty", {
              local_envvar(c("HOME" = paste0(devtools::package_file(), "/tests/.tmp_home/.can_write")))
              expect_error(suppressWarnings(download_model(model_url = "example.com/dawnn_nn_model.h5")),
                           "cannot open URL 'example.com/dawnn_nn_model.h5'")

              # Delete test .dawnn directory
              unlink(paste0(Sys.getenv("HOME"), "/.dawnn"), recursive = TRUE)
})
