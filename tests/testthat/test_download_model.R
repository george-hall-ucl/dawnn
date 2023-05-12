# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

create_tmp_home_dir <- function(writable = TRUE, env = parent.frame()) {
    # Create a new directory at "tempdir()/home_dir"
    dir_path <- file.path(tempdir(), "home_dir")
    dir.create(dir_path)
    if (writable == FALSE) {
        Sys.chmod(dir_path, mode = "577") # Make unwritable
    }
    withr::defer(unlink(dir_path, recursive = TRUE), envir = env)

    return(dir_path)
}


test_that("download_model functions with no arguments", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    expected_model_path <- paste0(normalizePath(Sys.getenv("HOME"),
                                                mustWork = FALSE),
                                  "/.dawnn/dawnn_nn_model.h5")
    expected_msg <- paste("Model was downloaded to:", expected_model_path)
    expect_message(download_model(), expected_msg)
})


test_that("download_model saves model in correct location", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    desired_model_path <- paste0(normalizePath(Sys.getenv("HOME"),
                                               mustWork = FALSE),
                                 "/.dawnn/my_path.h5")

    expected_msg <- paste("Model was downloaded to:",
                          normalizePath(desired_model_path, mustWork = FALSE))
    expect_message(download_model(model_file_path = desired_model_path),
                   expected_msg)
    expect_equal(file.exists(desired_model_path), TRUE)
})


test_that("download_model stops if cannot create .dawnn", {
    local_envvar(c("HOME" = create_tmp_home_dir(writable = FALSE)))
    expect_error(suppressWarnings(download_model()),
                 "Not downloading as cannot create ~/.dawnn directory")
})


test_that("download_model warns if downloaded file is smaller than expected", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    expect_warning(download_model(model_url = "http://www.example.com"),
                   "Downloaded model file is different to expected size: wrong file?")
})


test_that("download_model stops if URL is faulty", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    expect_error(suppressWarnings(download_model(model_url = "http://www.example.com/dawnn_nn_model.h5")),
                 "cannot open the connection to 'http://www.example.com/dawnn_nn_model.h5'")
})


test_that("download_model detects if timeout too small", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    # "fixed = TRUE" ensures that the question mark is not used as a
    # special character
    expect_error(suppressWarnings(download_model(download_timeout = 1)),
                 paste("Error in model download, perhaps due to timeout?",
                       "Try increasing download_timeout parameter.",
                       collapse = " "), fixed = TRUE)
})
