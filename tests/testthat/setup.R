# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

library(withr)
library(Seurat)

sm <- suppressMessages
sep_r <- function(x, args = list(), print_stdout = TRUE, print_stderr = FALSE, ...) {
    res <- callr::r(x, args = args, stdout = "/tmp/out", stderr = "/tmp/err")
    if (print_stdout) {
        outs <- readLines("/tmp/out")
        if (!identical(outs, character(0))) {
            print(outs)
        }
    }
    if (print_stderr) {
        outs <- readLines("/tmp/err")
        if (!identical(outs, character(0))) {
            print(outs)
        }
    }
    return(res)
}
