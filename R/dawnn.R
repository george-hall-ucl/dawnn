# Copyright (C) 2023-2026 University College London
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Estimate the parameters of a beta distribution using the method of moments.
#'
#' @param data Vector of numbers for which to estimate the parameters.
#' @return A list containing the two parameters of the fitted beta distribution.
#' @keywords internal
#' @examples
#' \dontrun{
#' set.seed(123)
#' beta_sample <- rbeta(10000, shape1 = 2, shape2 = 5)
#' beta_method_of_moments(beta_sample)
#' # $alpha
#' # [1] 1.982009
#' #
#' # $beta
#' # [1] 4.942666
#' }
beta_method_of_moments <- function(data) {
    sample_mean <- mean(data)
    sample_var <- var(data)
    common_factor <- ((sample_mean * (1 - sample_mean)) / sample_var) - 1

    # A beta distribution can only be fitted by the method of moments when the
    # sample variance is smaller than mean * (1 - mean). Otherwise both shape
    # parameters come out non-positive and every subsequent pbeta() call
    # silently returns NaN.
    if (!is.finite(common_factor) || common_factor <= 0) {
        stop(paste(
            "Cannot fit a beta distribution to these data by the",
            "method of moments: the sample variance is too large",
            "relative to the sample mean."
        ))
    }

    alpha <- sample_mean * common_factor
    beta <- (1 - sample_mean) * common_factor

    return(list(alpha = alpha, beta = beta))
}

#' Generate a matrix of the labels of the 1,000 nearest neighbors of each cell.
#'
#' @param cells Seurat object containing the dataset.
#' @param verbose Boolean verbosity.
#' @param label_names String containing the name of the meta.data slot in
#' `cells` containing the labels of each cell.
#' @param label_pos_lfc String containing the name of the label associated with
#' positive log-fold change.
#' @return A data frame containing the labels of the 1000 nearest neighbors of
#' each cell.
#' @keywords internal
#' @examples
#' \dontrun{
#' generate_neighbor_labels(cell_object,
#'     verbose = TRUE, label_names =
#'         "sample_names", label_pos_lfc = "Condition1"
#' )
#' }
generate_neighbor_labels <- function(cells, verbose, label_names, label_pos_lfc) {
    if (verbose) {
        message("Creating adjacency matrix.")
    }
    graph_name <- names(cells@neighbors)[1]
    if (length(names(cells@neighbors)) > 1) {
        message(paste(
            "Multiple available graph names. Choosing the first one:",
            graph_name
        ))
    }
    nn_idx <- cells@neighbors[[graph_name]]@nn.idx
    # The [-1] in the anonymous function below drops each cell's first
    # neighbor. This approach is only correct if the first neighbor is the cell
    # itself. This should be the case in the graph constructed by Seurat's
    # FindNeighbors, but may not be true in general. We check that this
    # assumption is satisfied before proceeding.
    if (!identical(as.integer(nn_idx[, 1]), seq_len(nrow(nn_idx)))) {
        stop(paste(
            "Dawnn requires each cell to be its own first nearest",
            "neighbor, which is not the case in this graph."
        ))
    }
    nhbor_labels_mtx <- apply(
        nn_idx, 1,
        function(x) {
            cells@meta.data[[label_names]][x][-1]
        }
    )
    nhbor_labels_df <- data.frame(nhbor_labels_mtx)
    nhbor_labels_binary_df <- nhbor_labels_df == label_pos_lfc
    nhbor_labels_binary_mtx <- apply(nhbor_labels_binary_df, 1, as.numeric)

    return(nhbor_labels_binary_mtx)
}


#' Load the neural network model from its .hdf5 file.
#'
#' @param model_path String containing the path to the model's .hdf5 file.
#' @return The loaded model.
#' @keywords internal
#' @examples
#' \dontrun{
#' nn_model <- load_model_from_python("/path/to/the/model.hdf5")
#' }
load_model_from_python <- function(model_path) {
    # Need to have tensorflow installed in the reticulate environment. Check
    # whether it is installed:
    if (!py_module_available("tensorflow")) {
        stop(paste(
            "Tensorflow not installed. If you have Tensorflow installed in a",
            "conda environment, you can make Dawnn use this with the",
            "`tf_conda_env` parameter of `run_dawnn`."
        ))
    }

    if (!file.exists(model_path)) {
        stop(paste0(
            "No model available at ", model_path,
            ": run download_model() to download it."
        ))
    }
    # load model trained with Python
    model <- load_model_hdf5(model_path, compile = FALSE)

    return(model)
}


#' Generate a null distribution of P(Condition_1) estimates.
#'
#' @description `generate_null_dist()` shuffles the sample labels three times
#' and returns the estimates of P(Condition_1) for each shuffled dataset.
#'
#' @param cells Seurat object containing the dataset.
#' @param model Loaded neural network model to use.
#' @param label_names String containing the name of the meta.data slot in
#' `cells` containing the labels of each cell.
#' @param label_pos_lfc String containing the name of the label associated with
#' positive log-fold change.
#' @param verbosity Integer how much output to print. 0: silent; 1: normal
#' output; 2: display messages from predict() function.
#' @param da_mode String containing the type of differential abundance being
#' sought, either "lda" (local DA) or "gda" (global DA).
#' @return A vector containing a null distribution of Dawnn's model outputs for
#' shuffled sample labels.
#' @keywords internal
#' @examples
#' \dontrun{
#' generate_null_dist(
#'     cells = cell_object, model = nn_model, label_names =
#'         "synth_labels", label_pos_lfc = "Condition_1", verbosity = 1, da_mode = "lda"
#' )
#' }
generate_null_dist <- function(cells, model, label_names, label_pos_lfc, verbosity,
                               da_mode = c("lda", "gda")) {
    # Without this, an unrecognised da_mode silently falls through to the
    # "gda" branch below rather than erroring.
    da_mode <- match.arg(da_mode)

    null_dist <- c()
    for (i in 1:3) {
        num_cells <- ncol(cells)
        labels <- cells@meta.data[, label_names]
        if (da_mode == "lda") {
            label_neg_lfc <- setdiff(unique(labels), label_pos_lfc)
            labels <- c(
                rep(label_pos_lfc, round(num_cells / 2)),
                rep(label_neg_lfc, num_cells - round(num_cells / 2))
            )
        }
        # Sort the labels to ensure that the same result is returned for both
        # LDA and GDA if Condition1 and Condition2 are in equal proportions.
        # Since we then shuffle them, this does not make the result any more or
        # less correct than not sorting them, but it ensures the above goal is
        # satisfied if the labels are not arranged identically in both cases.
        labels <- sort(labels)
        cells$shuff_labels <- sample(labels)
        shuff_nbor_labs <- generate_neighbor_labels(cells,
            label_names = "shuff_labels",
            label_pos_lfc = label_pos_lfc,
            verbose = verbosity > 0
        )
        shuff_scores <- model$predict(shuff_nbor_labs,
            verbose = ifelse(verbosity == 2, 1, 0)
        )
        null_dist <- c(null_dist, shuff_scores)
    }

    return(null_dist)
}


#' Generate p-values for observed Dawnn model outputs.
#'
#' @description `generate_p_vals()` takes Dawnn model outputs and a null
#' distribution and returns p-values of the observed outputs.
#'
#' @param scores Numeric vector containing observed output of Dawnn.
#' @param null_dist Numeric vector containing null distribution of scores.
#' @return Numeric vector containing a p-value for each cell, i.e. the
#' probability of observing at least such an extreme score for a cell given the
#' beta distribution fitted to the null distribution of scores.
#' @keywords internal
#' @examples
#' \dontrun{
#' generate_p_vals(scores = score_vect, null_dist = null_scores)
#' }
generate_p_vals <- function(scores, null_dist) {
    null_dist_est_params <- beta_method_of_moments(null_dist)
    null_alpha <- null_dist_est_params$alpha
    null_beta <- null_dist_est_params$beta
    null_mode <- (null_alpha - 1) / (null_alpha + null_beta - 2)

    p_vals <- c()

    for (score in scores) {
        if (score <= null_mode) {
            p_vals <- c(p_vals, pbeta(score, null_alpha, null_beta))
        } else {
            p_vals <- c(p_vals, 1 - pbeta(score, null_alpha, null_beta))
        }
    }

    return(p_vals)
}


#' Determine whether each cell is in a region of differential abundance.
#'
#' @description `determine_if_region_da()` takes a vector of p-values and uses
#' the Benjamini–Yekutieli procedure to determine whether a cell is in a
#' region of differential abundance.
#'
#' @param p_vals Numeric vector of p-values.
#' @param alpha Numeric target false discovery rate supplied to the
#' Benjamini–Yekutieli procedure.
#' @return Boolean vector containing Dawnn's verdict for each cell.
#' @keywords internal
#' @examples
#' \dontrun{
#' determine_if_region_da(p_vals = p_value_vector, alpha = 0.2)
#' }
determine_if_region_da <- function(p_vals, alpha) {
    num_cells <- length(p_vals)

    c <- 0
    for (k in 1:num_cells) {
        c <- (c + (1 / k))
    }

    da_verdict <- rep(FALSE, num_cells)
    j <- 1
    for (i in order(p_vals)) {
        # This is the "Benjamini–Yekutieli procedure", which allows for
        # arbitrary dependence assumptions. We can remove the "c" if we assume
        # that all tests are independent.
        cutoff <- (j * alpha) / (num_cells * c)

        if (p_vals[i] <= cutoff) {
            da_verdict[i] <- TRUE
        } else {
            break
        }

        j <- j + 1
    }

    return(da_verdict)
}


#' Default path where Dawnn stores its downloaded model.
#'
#' @return String path to the model file.
#' @keywords internal
dawnn_default_model_file <- function() {
    file.path(
        tools::R_user_dir("dawnn", which = "cache"),
        "dawnn_nn_model.h5"
    )
}


#' Check a downloaded model against its expected size and checksum.
#'
#' @description `check_model_file()` warns if the file at `model_file_path` is
#' not the model expected at Dawnn's default URL. The checksum is only compared
#' when the size matches, so a truncated download produces one warning rather
#' than two.
#'
#' @param model_file_path String path to the downloaded model.
#' @param expected_size Integer expected size of the model, in bytes.
#' @param expected_md5 String expected MD5 checksum of the model.
#' @return Invisibly, `TRUE` if the file matches and `FALSE` otherwise.
#' @keywords internal
check_model_file <- function(model_file_path, expected_size, expected_md5) {
    downloaded_size <- file.info(model_file_path)$size
    if (is.na(downloaded_size) || downloaded_size != expected_size) {
        warning("Downloaded model file is different to expected size: wrong file?")
        return(invisible(FALSE))
    }

    if (!identical(unname(tools::md5sum(model_file_path)), expected_md5)) {
        warning("Downloaded model does not have the expected MD5 checksum: wrong file?")
        return(invisible(FALSE))
    }

    return(invisible(TRUE))
}


#' Download the neural network model used by Dawnn.
#'
#' @description `download_model()` downloads the neural network model used by
#' Dawnn, which is too large to be bundled with the package. This function must
#' be used once before run_dawnn() can be executed. After this, the path to the
#' model can be passed to this function.
#'
#' @param model_url String url from which to download the model.
#' @param model_file_path String path at which to save the downloaded model.
#' @param download_method String download program to use (e.g. wget, curl etc).
#' @param download_timeout Integer number of seconds before download times out
#' (optional, default = 600).
#' @return Message confirming the absolute path to the downloaded model.
#' @examples
#' \dontrun{
#' model_path <- download_model()
#' }
#' @export
download_model <- function(model_url = NULL, model_file_path = NULL,
                           download_method = "auto", download_timeout = 600) {
    # Size in bytes and MD5 checksum of the model hosted at the default URL.
    # Used as sanity check of default model.
    expected_model_size <- 255225824
    expected_model_md5 <- "e40f36e2a625f137a865e806a188b6aa"

    using_default_url <- is.null(model_url)
    if (using_default_url) {
        model_url <- "https://zenodo.org/records/21804035/files/final_model_dawnn_rerun.h5"
    }

    using_default_path <- is.null(model_file_path)
    if (using_default_path) {
        model_file_path <- dawnn_default_model_file()
        dawnn_dir_path <- dirname(model_file_path)
    } else {
        dawnn_dir_path <- dirname(normalizePath(model_file_path,
            mustWork = FALSE
        ))
    }

    if (dir.exists(dawnn_dir_path) == FALSE) {
        # Get consent before creating directory
        if (using_default_path && interactive()) {
            answer <- readline(paste0(
                "Create ", dawnn_dir_path,
                " to store Dawnn's model? [y/N] "
            ))
            if (!identical(tolower(substr(answer, 1, 1)), "y")) {
                stop(
                    "Not downloading as permission to create ",
                    dawnn_dir_path, " was declined"
                )
            }
        }
        dir_create_ret <- dir.create(dawnn_dir_path, recursive = TRUE)
        if (dir_create_ret != TRUE) {
            stop("Not downloading as cannot create directory ", dawnn_dir_path)
        }
    }
    message(paste(
        "Downloading Dawnn's neural network model to",
        model_file_path
    ))

    # Check if url exists
    if (!grepl("^[a-z][a-z0-9+.-]*://", model_url)) {
        # Prevent error with url() if protocol is unspecified
        model_url <- paste0("http://", model_url)
    }

    con <- url(model_url, headers = list("test" = "test"))
    open.connection(con, open = "rt", timeout = 2)
    close(con, silent = TRUE)

    # Increase timeout to 10 minutes
    old_timeout <- getOption("timeout")
    options(timeout = download_timeout)

    tryCatch(
        download_ret <- download.file(model_url, model_file_path,
            method = download_method,
            mode = "wb"
        ),
        error = function(c) {
            options(timeout = old_timeout)
            stop("Error in model download, perhaps due to timeout? Try increasing download_timeout parameter.")
        }
    )

    options(timeout = old_timeout)

    if (download_ret != 0) {
        stop(paste("Download finished with non-zero exit code:", download_ret))
    }

    if (using_default_url) {
        check_model_file(
            model_file_path, expected_model_size,
            expected_model_md5
        )
    }

    return_msg <- paste(
        "Model was downloaded to:",
        normalizePath(model_file_path)
    )
    message(return_msg)

    return(return_msg)
}


#' Sanity check input parameters
#'
#' @description `param_check()` verifies that the parameters passed to
#' run_dawnn() are sane.
#'
#' @param cells Seurat object containing the dataset.
#' @param label_names String containing the name of the meta.data slot in
#' `cells` containing the labels of each cell.
#' @param label_pos_lfc String containing the name of the label associated with
#' positive log-fold change.
#' @param reduced_dim String containing the name of the dimensionality
#' reduction to use.
#' @param recalculate_graph Boolean whether to recalculate the KNN graph. If
#' FALSE, then the one stored in the `cells` object will be used (optional,
#' default = TRUE).
#' @return TRUE if all parameters sane, otherwise stop execution with error
#' message.
#' @keywords internal
#' @examples
#' \dontrun{
#' param_check(cells, label_names, label_pos_lfc, reduced_dim, recalculate_graph)
#' }
param_check <- function(cells, label_names, label_pos_lfc, reduced_dim,
                        recalculate_graph) {
    # Does label_names name a meta.data column?
    if (!label_names %in% colnames(cells@meta.data)) {
        stop(paste("No meta.data column:", label_names))
    }

    # Are any labels missing?
    if (any(is.na(cells[[label_names]][, 1]))) {
        stop("Labels must not contain NA.")
    }

    # Are there two unique labels?
    if (length(unique(cells[[label_names]][, 1])) != 2) {
        stop("There must be exactly two distinct labels.")
    }

    # Does label_pos_lfc appear in the set of labels?
    if (!label_pos_lfc %in% cells[[label_names]][, 1]) {
        stop("label_pos_lfc must be assigned to at least one cell.")
    }

    # Does reduced_dim exist?
    if (!reduced_dim %in% names(cells@reductions)) {
        stop(paste("No dimensionality reduction:", reduced_dim))
    }

    # Does a KNN graph exist?
    if ((recalculate_graph == FALSE) & (length(cells@neighbors) == 0)) {
        stop(paste(
            "No K-nearest-neighbor graph but recalculate_graph is",
            "FALSE. Set to TRUE or run Seurat::FindNeighbors()."
        ))
    }

    return(TRUE)
}


#' Identify which cells are in regions of differential abundance using Dawnn.
#'
#' @description `run_dawnn()` is the main function used to run Dawnn. It takes
#' a Seurat dataset and identifies which cells are in regions of differential
#' abundance. Dawnn requires at least 1,001 cells.
#'
#' @param cells Seurat object containing the dataset.
#' @param label_names String containing the name of the meta.data slot in
#' `cells` containing the labels of each cell.
#' @param label_pos_lfc String containing the name of the label associated with
#' positive log-fold change.
#' @param reduced_dim String containing the name of the dimensionality
#' reduction to use.
#' @param n_dims Integer number of dimensions to use if computing graph
#' (optional, default 10).
#' @param nn_model String containing the path to the model's .hdf5 file
#' (optional, defaults to the location used by [download_model()]).
#' @param recalculate_graph Boolean whether to recalculate the KNN graph. If
#' FALSE, then the one stored in the `cells` object will be used (optional,
#' default = TRUE).
#' @param alpha Numeric target false discovery rate supplied to the
#' Benjamini–Yekutieli procedure (optional, default 0.1, i.e. 10%).
#' @param verbosity Integer how much output to print. 0: silent; 1: normal
#' output; 2: display messages from predict() function.
#' @param seed Integer random seed (optional, default 123).
#' @param tf_conda_env Conda environment with TensorFlow installed, useful if
#' it is unavailable in the current environment (optional, default NULL).
#' @return Seurat dataset `cells` with added metadata: `dawnn_scores` (output
#' of Dawnn's model for each cell); `dawnn_lfc` (estimated log2-fold change in
#' the neighbourhood of each cell); `dawnn_p_vals_lda` and `dawnn_p_vals_gda`
#' (p-values associated with the hypothesis tests for whether a cell is in a
#' region of local or global differential abundance, respectively);
#' `dawnn_lda_verdict` and `dawnn_gda_verdict` (Boolean output of Dawnn
#' indicating whether it considers a cell to be in a region of local or
#' global differential abundance, respectively).
#' @examples
#' \dontrun{
#' run_dawnn(
#'     cells = dataset, label_names = "condition", label_pos_lfc = "Condition_1",
#'     nn_model = "my_model.h5", reduced_dim = "pca", n_dims = 50,
#'     recalculate_graph = FALSE, alpha = 0.2, verbosity = 0, seed = 42,
#'     tf_conda_env = "my_tensorflow_env"
#' )
#' }
#' @export
run_dawnn <- function(cells, label_names, label_pos_lfc, reduced_dim,
                      n_dims = 10, nn_model = dawnn_default_model_file(),
                      recalculate_graph = TRUE, alpha = 0.1, verbosity = 2,
                      seed = 123, tf_conda_env = NULL) {
    withr::local_seed(seed)

    if (!is.null(tf_conda_env)) {
        reticulate::use_condaenv(tf_conda_env)
    }

    num_cells <- ncol(cells)
    if (num_cells < 1001) {
        stop(paste0(
            "Dawnn requires at least 1001 cells. Your dataset contains ",
            num_cells, "."
        ))
    }

    param_check(cells, label_names, label_pos_lfc, reduced_dim, recalculate_graph)

    if (class(nn_model)[1] == "character") {
        nn_model <- load_model_from_python(nn_model)
    }

    if (recalculate_graph) {
        if (verbosity > 0) {
            message("Finding neighbors.")
        }
        cells <- FindNeighbors(cells,
            dims = (1:n_dims),
            return.neighbor = TRUE, k.param = 1001,
            reduction = reduced_dim
        )
    }

    if (verbosity > 0) {
        message("Generating neighbor labels.")
    }
    neighbor_labels <- generate_neighbor_labels(cells,
        label_names = label_names,
        label_pos_lfc = label_pos_lfc,
        verbose = verbosity > 0
    )

    if (verbosity > 0) {
        message("Generating scores.")
    }
    scores <- nn_model$predict(neighbor_labels,
        verbose = ifelse(verbosity == 2, 1, 0)
    )
    cells$dawnn_scores <- scores
    cells$dawnn_lfc <- log2(scores / (1 - scores))

    for (da_mode in c("lda", "gda")) {
        if (verbosity > 0) {
            message(paste(
                "Testing for",
                ifelse(da_mode == "lda", "local", "global"),
                "differential abundance."
            ))
            message("... Generating null distribution.")
        }
        withr::local_seed(seed)
        null_dist <- generate_null_dist(cells, nn_model, label_names, label_pos_lfc,
            verbosity = verbosity, da_mode = da_mode
        )

        if (verbosity > 0) {
            message("... Generating p-values.")
        }
        p_vals <- generate_p_vals(scores, null_dist)
        cells@meta.data[[paste0("dawnn_p_vals_", da_mode)]] <- p_vals

        if (verbosity > 0) {
            message("... Determining significance.")
        }
        verdicts <- determine_if_region_da(p_vals, alpha = alpha)
        cells@meta.data[[paste0("dawnn_", da_mode, "_verdict")]] <- verdicts
    }

    return(cells)
}
