# Copyright (C) 2023 University College London
# Licensed under GNU GPL Version 3 <https://www.gnu.org/licenses/gpl-3.0.html>

library(withr)
library(Seurat)

sm <- suppressMessages
sep_r <- function(x, print_stdout = TRUE, print_stderr = FALSE) {
    res <- callr::r(x, stdout = "/tmp/out", stderr = "/tmp/err")
    if (print_stdout) {
        print(readLines("/tmp/out"))
    }
    if (print_stderr) {
        print(readLines("/tmp/err"))
    }
    return(res)
}
