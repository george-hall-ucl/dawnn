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


# Creates a small local file to stand in for the real (~255MB) production
# model, and returns a "file://" URL pointing at it. Passing this as
# `model_url` exercises the entire real download_model() code path (url(),
# open.connection(), download.file(), size check, messages) with no network
# access, instead of mocking out the network-facing functions it calls.
create_tmp_source_file <- function(contents = "dawnn test model contents",
                                    env = parent.frame()) {
    file_path <- tempfile()
    writeLines(contents, file_path)
    withr::defer(unlink(file_path), envir = env)

    return(paste0("file://", file_path))
}


test_that("download_model downloads successfully to default location", {
    local_envvar(c("HOME" = create_tmp_home_dir()))

    expected_model_path <- paste0(normalizePath(Sys.getenv("HOME"),
                                                mustWork = FALSE),
                                  "/.dawnn/dawnn_nn_model.h5")
    expected_msg <- paste("Model was downloaded to:", expected_model_path)
    expect_message(download_model(model_url = create_tmp_source_file()),
                   expected_msg)
    expect_equal(file.exists(expected_model_path), TRUE)
})


test_that("download_model saves model in correct location", {
    local_envvar(c("HOME" = create_tmp_home_dir()))

    desired_model_path <- paste0(normalizePath(Sys.getenv("HOME"),
                                               mustWork = FALSE),
                                 "/.dawnn/my_path.h5")

    expected_msg <- paste("Model was downloaded to:",
                          normalizePath(desired_model_path, mustWork = FALSE))
    expect_message(download_model(model_url = create_tmp_source_file(),
                                  model_file_path = desired_model_path),
                   expected_msg)
    expect_equal(file.exists(desired_model_path), TRUE)
})


test_that("download_model stops if cannot create .dawnn", {
    local_envvar(c("HOME" = create_tmp_home_dir(writable = FALSE)))
    expect_error(suppressWarnings(download_model()),
                 "Not downloading as cannot create ~/.dawnn directory")
})


test_that("download_model does not size-check a user-supplied model_url", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    expect_no_warning(download_model(model_url = create_tmp_source_file()))
})


test_that("download_model stops if URL is faulty", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    faulty_url <- paste0("file://", tempfile())

    expect_error(suppressWarnings(download_model(model_url = faulty_url)),
                 "cannot open the connection")
})


test_that("download_model detects if timeout too small", {
    local_envvar(c("HOME" = create_tmp_home_dir()))
    # A real network timeout can't be simulated with a local file:// URL, so
    # this test alone keeps a narrow mock of download.file() -- the only one
    # of the two network-facing calls that is actually mockable, since it is
    # imported (importFrom(utils, download.file) in NAMESPACE) while
    # open.connection() lives in base and is not. It still exercises real
    # dawnn logic: the tryCatch wrapper around download.file() in
    # download_model().
    local_mocked_bindings(
        download.file = function(...) stop("simulated download timeout"),
        .package = "dawnn"
    )

    # "fixed = TRUE" ensures that the question mark is not used as a
    # special character
    expect_error(suppressWarnings(
                     download_model(model_url = create_tmp_source_file(),
                                    download_timeout = 1)),
                 paste("Error in model download, perhaps due to timeout?",
                       "Try increasing download_timeout parameter.",
                       collapse = " "), fixed = TRUE)
})
