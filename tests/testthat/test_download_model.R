# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

# download_model() stores the model under tools::R_user_dir("dawnn", "cache"),
# which reads R_USER_CACHE_DIR. Point that at a temporary directory so the
# tests never touch the real cache.
create_tmp_cache_dir <- function(env = parent.frame()) {
    dir_path <- file.path(tempdir(), "cache_dir")
    dir.create(dir_path, showWarnings = FALSE)
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


# Creates a small local file and returns its path, size and MD5 checksum, so
# that check_model_file() can be tested against values that really describe it.
create_tmp_model_file <- function(contents = "dawnn test model contents",
                                  env = parent.frame()) {
    file_path <- tempfile()
    writeLines(contents, file_path)
    withr::defer(unlink(file_path), envir = env)

    return(list(path = file_path,
                size = file.info(file_path)$size,
                md5 = unname(tools::md5sum(file_path))))
}


test_that("check_model_file accepts a matching file", {
    model <- create_tmp_model_file()

    expect_no_warning(result <- dawnn:::check_model_file(model$path,
                                                         model$size,
                                                         model$md5))
    expect_true(result)
})


test_that("check_model_file warns if the size is wrong", {
    model <- create_tmp_model_file()

    expect_warning(result <- dawnn:::check_model_file(model$path,
                                                      model$size + 1,
                                                      model$md5),
                   "different to expected size")
    expect_false(result)
})


test_that("check_model_file warns if the checksum is wrong", {
    model <- create_tmp_model_file()
    # The size is deliberately correct, so that reaching the checksum warning
    # proves the checksum itself was compared.
    wrong_md5 <- paste(rep("0", nchar(model$md5)), collapse = "")

    expect_warning(result <- dawnn:::check_model_file(model$path, model$size,
                                                      wrong_md5),
                   "MD5 checksum")
    expect_false(result)
})


test_that("check_model_file warns if the file is absent", {
    model <- create_tmp_model_file()
    unlink(model$path)

    # file.info()$size is NA here, so this also covers the is.na() guard.
    expect_warning(result <- dawnn:::check_model_file(model$path, model$size,
                                                      model$md5),
                   "different to expected size")
    expect_false(result)
})


test_that("download_model downloads successfully to default location", {
    local_envvar(c("R_USER_CACHE_DIR" = create_tmp_cache_dir()))

    expected_model_path <- file.path(tools::R_user_dir("dawnn", "cache"),
                                     "dawnn_nn_model.h5")
    expect_message(download_model(model_url = create_tmp_source_file()),
                   "Model was downloaded to:")
    expect_equal(file.exists(expected_model_path), TRUE)
})


test_that("download_model saves model in correct location", {
    desired_model_path <- file.path(create_tmp_cache_dir(), "my_path.h5")

    # Run once to ensure model file in correct location (needed to test message).
    suppressMessages(download_model(model_url = create_tmp_source_file(),
                                    model_file_path = desired_model_path))

    # "fixed = TRUE" ensures that the path is not treated as a regex.
    expected_msg <- paste("Model was downloaded to:",
                          normalizePath(desired_model_path, mustWork = FALSE))
    expect_message(download_model(model_url = create_tmp_source_file(),
                                  model_file_path = desired_model_path),
                   expected_msg, fixed = TRUE)
    expect_equal(file.exists(desired_model_path), TRUE)
})


test_that("download_model stops if it cannot create the model directory", {
    # A regular file blocks dir.create() for root too, unlike chmod.
    blocker <- tempfile()
    file.create(blocker)
    withr::defer(unlink(blocker))

    # model_url is local so a regression cannot start a real download.
    expect_error(suppressWarnings(
                     download_model(model_url = create_tmp_source_file(),
                                    model_file_path = file.path(blocker,
                                                                "model.h5"))),
                 "cannot create")
})


test_that("download_model does not size-check a user-supplied model_url", {
    local_envvar(c("R_USER_CACHE_DIR" = create_tmp_cache_dir()))
    expect_no_warning(download_model(model_url = create_tmp_source_file()))
})


test_that("download_model stops if URL is faulty", {
    local_envvar(c("R_USER_CACHE_DIR" = create_tmp_cache_dir()))
    faulty_url <- paste0("file://", tempfile())

    expect_error(suppressWarnings(download_model(model_url = faulty_url)),
                 "cannot open the connection")
})


test_that("download_model detects if timeout too small", {
    local_envvar(c("R_USER_CACHE_DIR" = create_tmp_cache_dir()))
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
