# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

library(withr)
library(Seurat)

sm <- suppressMessages


# Checks conda_list() rather than use_condaenv(), which would bind Python for
# the whole process.
skip_if_no_dawnn_deps <- function(env = "tf_env",
                                  model = dawnn:::dawnn_default_model_file()) {
    skip_on_cran()
    skip_if_not_installed("devtools")
    skip_if_not_installed("reticulate")
    conda_envs <- tryCatch(reticulate::conda_list()$name,
                           error = function(e) character(0))
    skip_if_not(env %in% conda_envs,
                paste0("conda environment '", env, "' not available"))
    skip_if_not(file.exists(model),
                paste("Dawnn model not found at", model))
}


sep_r <- function(x, args = list(), print_stdout = TRUE, print_stderr = FALSE, ...) {
    out_path <- tempfile("callr_stdout_")
    err_path <- tempfile("callr_stderr_")
    on.exit(unlink(c(out_path, err_path)), add = TRUE)

    res <- callr::r(x, args = args, stdout = out_path, stderr = err_path)
    if (print_stdout) {
        outs <- readLines(out_path)
        if (!identical(outs, character(0))) {
            print(outs)
        }
    }
    if (print_stderr) {
        outs <- readLines(err_path)
        if (!identical(outs, character(0))) {
            print(outs)
        }
    }
    return(res)
}
