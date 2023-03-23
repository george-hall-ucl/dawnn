test_that("download_model functions with no arguments", {
              old_home <- Sys.getenv("HOME")
              Sys.setenv(HOME = "/Users/georgehall/Documents/Code/dawnn_everything/dawnn/tests/.tmp_home/.can_write")

              home_dir <- Sys.getenv("HOME")
              dawnn_dir_path <- paste0(home_dir, "/.dawnn")
              expected_model_path <- paste0(dawnn_dir_path,
                                            "/dawnn_nn_model.h5")

              expected_msg <- paste("Model was downloaded to:",
                                    expected_model_path)
              expect_message(download_model(),
                             "Model was downloaded to: /Users/georgehall/Documents/Code/dawnn_everything/dawnn/tests/.tmp_home/.can_write/.dawnn/dawnn_nn_model.h5")

              # Delete test .dawnn directory
              unlink(dawnn_dir_path, recursive = TRUE)
              Sys.setenv(HOME = old_home)
})


test_that("download_model saves model in correct location", {
              old_home <- Sys.getenv("HOME")
              Sys.setenv(HOME = "/Users/georgehall/Documents/Code/dawnn_everything/dawnn/tests/.tmp_home/.can_write")
              home_dir <- Sys.getenv("HOME")
              dawnn_dir_path <- paste0(home_dir, "/.dawnn")
              desired_model_path <- paste0(dawnn_dir_path, "/my_path.h5")
              expected_msg <- paste("Model was downloaded to:",
                                    desired_model_path)
              expect_message(download_model(model_file_path = desired_model_path),
                             "Model was downloaded to: /Users/georgehall/Documents/Code/dawnn_everything/dawnn/tests/.tmp_home/.can_write/.dawnn/my_path.h5")
              expect_equal(file.exists(desired_model_path), TRUE)

              # Delete test .dawnn directory
              unlink(dawnn_dir_path, recursive = TRUE)
              Sys.setenv(HOME = old_home)
})


test_that("download_model stops if cannot create .dawnn", {
              old_home <- Sys.getenv("HOME")
              Sys.setenv(HOME = "/Users/georgehall/Documents/Code/dawnn_everything/dawnn/tests/.tmp_home/.cannot_write")
              expect_error(suppressWarnings(download_model()),
                           "Not downloading as cannot create ~/.dawnn directory")
              Sys.setenv(HOME = old_home)
})


test_that("download_model warns if downloaded file is smaller than expected", {
              old_home <- Sys.getenv("HOME")
              Sys.setenv(HOME = "/Users/georgehall/Documents/Code/dawnn_everything/dawnn/tests/.tmp_home/.can_write")
              home_dir <- Sys.getenv("HOME")
              dawnn_dir_path <- paste0(home_dir, "/.dawnn")
              desired_model_path <- paste0(dawnn_dir_path, "/my_path.h5")
              expect_warning(download_model(model_url = "example.com"),
                             "Downloaded model file is different to expected size: wrong file?")

              # Delete test .dawnn directory
              unlink(dawnn_dir_path, recursive = TRUE)
              Sys.setenv(HOME = old_home)
})


test_that("download_model stops if URL is faulty", {
              old_home <- Sys.getenv("HOME")
              Sys.setenv(HOME = "/Users/georgehall/Documents/Code/dawnn_everything/dawnn/tests/.tmp_home/.can_write")
              home_dir <- Sys.getenv("HOME")
              dawnn_dir_path <- paste0(home_dir, "/.dawnn")
              desired_model_path <- paste0(dawnn_dir_path, "/my_path.h5")
              expect_error(suppressWarnings(download_model(model_url = "example.com/dawnn_nn_model.h5")),
                           "cannot open URL 'example.com/dawnn_nn_model.h5'")

              # Delete test .dawnn directory
              unlink(dawnn_dir_path, recursive = TRUE)
              Sys.setenv(HOME = old_home)
})
