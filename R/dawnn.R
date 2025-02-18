# Copyright (C) 2023 University College London
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
#' @param data Vector of numbers to which for which to estimate the parameters.
#' @return A list containing the two parameters of the fitted beta ditribution.
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

    alpha <- sample_mean * common_factor
    beta <- (1 - sample_mean) * common_factor

    return(list(alpha = alpha, beta = beta))
}

#' Generate a matrix of the labels of the 1,000 nearest neighbors of each cell.
#'
#' @param cells Seurat object containing the dataset.
#' @param verbose Boolean verbosity.
#' @param label_names String containing the name of the meta.data slot in
#' `cells' containing the labels of each cell.
#' @param label_1 String containing the name of one of the labels.
#' @return A data frame containing the labels of the 1000 nearest neighbors of
#' each cell.
#' @examples
#' \dontrun{
#' generate_neighbor_labels(cell_object, verbose = TRUE, label_names =
#' "sample_names", label_1 = "Condition1")
#' }
generate_neighbor_labels <- function(cells, verbose, label_names, label_1) {

    if (verbose) {
        message("Creating adjacency matrix.")
    }
    graph_name <- names(cells@neighbors)[1]
    if (length(names(cells@neighbors)) > 1) {
        message(paste("Multiple available graph names. Choosing the first one:",
                      graph_name))
    }
    nhbor_labels_mtx <- apply(cells@neighbors[[graph_name]]@nn.idx, 1,
                              function(x) {
                                  cells@meta.data[[label_names]][x][-1]
                              })
    nhbor_labels_df <- data.frame(nhbor_labels_mtx)
    nhbor_labels_binary_df <- nhbor_labels_df == label_1
    nhbor_labels_binary_mtx <- apply(nhbor_labels_binary_df, 1, as.numeric)

    return(nhbor_labels_binary_mtx)
}


#' Load the neural network model from its .hdf5 file.
#'
#' @param model_path String containing the path to the model's .hdf5 file.
#' @return The loaded model.
#' @examples
#' \dontrun{
#' nn_model <- load_model_from_python("/path/to/the/model.hdf5")
#' }
load_model_from_python <- function(model_path) {
    # Need to have tensorflow installed in the reticulate environment. Check
    # whether it is installed:
    if (py_module_available("tensorflow") == FALSE) {
        # I found it best to do this using conda.
        # conda_env_name <- "r-reticulate"
        # conda_create(conda_env_name)
        # conda_install(conda_env_name, "tensorflow")
        stop(paste("Tensorflow not installed in reticulate environment.",
                   "Please install following",
                   "rstudio.github.io/reticulate/articles/python_packages.html."))
    }

    if (!file.exists(model_path)) {
        stop(paste("No model available at", model_path))
    }
    # load model trained with Python
    model <- load_model_hdf5(model_path)

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
#' `cells' containing the labels of each cell.
#' @param label_1 String containing the name of one of the labels.
#' @param label_2 String containing the name of the other label.
#' @param verbosity Integer how much output to print. 0: silent; 1: normal
#' output; 2: display messages from predict() function.
#' @return A vector containing a null distribution of Dawnn's model outputs for
#' shuffled sample labels.
#' @examples
#' \dontrun{
#' generate_null_dist(cells = cell_object, model = nn_model, label_names =
#' "synth_labels", verbosity = 1)
#' }
generate_null_dist <- function(cells, model, label_names, label_1, label_2,
                               verbosity) {
    null_dist <- c()
    for (i in 1:3) {
        num_cells <- ncol(cells)
        labels <- c(rep(label_1, round(num_cells / 2)),
                    rep(label_2, num_cells - round(num_cells / 2)))
        cells$shuff_labels <- sample(labels)
        shuff_nbor_labs <- generate_neighbor_labels(cells,
                                                    label_names = "shuff_labels",
                                                    label_1 = label_1,
                                                    verbose = verbosity > 0)
        shuff_scores <- model$predict(shuff_nbor_labs,
                                      verbose = ifelse(verbosity == 2, 1, 0))
        null_dist <- c(null_dist, shuff_scores)
    }

    return(null_dist)
}


#' Generate p-values for observed Dawnn model outputs.
#'
#' @description `generate_null_dist()` takes Dawnn model outputs and a null
#' distribution and returns p-values of the observed outputs.
#'
#' @param scores Numeric vector containing observed output of Dawnn.
#' @param null_dist Numeric vector containing null distribution of scores.
#' @return Numeric vector containing a p-value for each cell, i.e. the
#' probability of observing at least such an extreme score for a cell given the
#' beta distribution fitted to the null distribution of scores.
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
#' @description `determine_if_region_da()` takes vectors of p-values,
#' observed scores, and the null distribution of scores and uses the
#' Benjamini–Yekutieli procedure to determine whether a cell is in a region of
#' differential abundance.
#'
#' @param p_vals Numeric vector of p-values.
#' @param scores Numeric vector containing observed output of Dawnn.
#' @param null_dist Numeric vector containing the null distribution of scores.
#' @param alpha Numeric target false discovery rate supplied to the
#' Benjamini–Yekutieli procedure.
#' @return Boolean vector containing Dawnn's verdict for each cell.
#' @examples
#' \dontrun{
#' determine_if_region_da(p_vals = p_value_vector, null_dist = null_scores,
#' alpha = 0.2)
#' }
determine_if_region_da <- function(p_vals, scores, null_dist, alpha) {
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


#' Download the neural network model used by Dawnn.
#'
#' @description `download_model()' downloads the neural network model used by
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
#' model_path <- download_model(model_url = "http://example.com/model.h5")
#' cells <- run_dawnn(cells, nn_model = model_path, [...])
#' }
#' @export
download_model <- function(model_url = NULL, model_file_path = NULL,
                           download_method = "auto", download_timeout = 600) {
    if (is.null(model_url)) {
        model_url <- "https://rdr.ucl.ac.uk/ndownloader/files/40162366"
    }

    if (is.null(model_file_path)) {
        # Unless told otherwise, save model in directory ".dawnn" in user's
        # home directory
        home_dir <- Sys.getenv("HOME")
        dawnn_dir_path <- paste0(home_dir, "/.dawnn")
        model_file_path <- paste0(dawnn_dir_path, "/dawnn_nn_model.h5")
    } else {
        dawnn_dir_path <- dirname(normalizePath(model_file_path,
                                                mustWork = FALSE))
    }

    if (dir.exists(dawnn_dir_path) == FALSE) {
        # If necessary, create model directory (by default, "~/.dawnn")
        dir_create_ret <- dir.create(dawnn_dir_path, recursive = TRUE)
        if (dir_create_ret != TRUE) {
            stop("Not downloading as cannot create ~/.dawnn directory")
        }
    }
    message(paste("Downloading Dawnn's neural network model to",
                  model_file_path))

    # Check if url exists
    if (substr(model_url, 1, 4) != "http") {
        # Prevent error with url() if protocol is unspecified
        model_url <- paste0("http://", model_url)
    }
    con <- url(model_url)
    open.connection(con, open = "rt", timeout = 2)
    close(con, silent = TRUE)

    # Increase timeout to 10 minutes
    old_timeout <- getOption("timeout")
    options(timeout = download_timeout)

    tryCatch(download_ret <- download.file(model_url, model_file_path, method = download_method),
             error = function(c) {
                 options(timeout = old_timeout)
                 stop("Error in model download, perhaps due to timeout? Try increasing download_timeout parameter.")
             })

    options(timeout = old_timeout)

    if (download_ret != 0) {
        stop(paste("Download finished with non-zero exit code:", download_ret))
    }

    if (file.info(model_file_path)$size != 255225824) {
        warning("Downloaded model file is different to expected size: wrong file?")
    }

    return_msg <- paste("Model was downloaded to:",
                        normalizePath(model_file_path))
    message(return_msg)

    return(return_msg)
}


#' Sanity check input parameters
#'
#' @description `param_check()' verifies that the parameters passed to
#' run_dawnn() are sane.
#'
#' @param cells Seurat object containing the dataset.
#' @param label_names String containing the name of the meta.data slot in
#' `cells' containing the labels of each cell.
#' @param label_1 String containing the name of one of the labels.
#' @param label_2 String containing the name of the other label.
#' @param reduced_dim String containing the name of the dimensionality
#' reduction to use.
#' @param recalculate_graph Boolean whether to recalculate the KNN graph. If
#' FALSE, then the one stored in the `cells` object will be used (optional,
#' default = TRUE).
#' @return TRUE if all parameters sane, otherwise stop execution with error
#' message.
#' @examples
#' \dontrun{
#' param_check(cells, label_names, label_1, label_2, reduced_dim,
#' recalculate_graph)
#' }
param_check <- function(cells, label_names, label_1, label_2, reduced_dim,
                        recalculate_graph) {
    # Are label_1 and label_2 distinct?
    if (label_1 == label_2) {
        stop("label_1 and label_2 must not be the same.")
    }

    # Are there two unique labels?
    if (length(unique(cells[[label_names]][, 1])) != 2) {
        stop("There must be exactly two distinct labels.")
    }

    # Do label_1 and label_2 both appear in the set of labels?
    if (!all(c(label_1, label_2) %in% unique(cells[[label_names]][, 1]))) {
        stop("Both label_1 and label_2 must be assigned to at least one cell.")
    }

    # Does reduced_dim exist?
    if (!reduced_dim %in% names(cells@reductions)) {
        stop(paste("No dimensionality reduction:", reduced_dim))
    }

    # Does a KNN graph exist?
    if ((recalculate_graph == FALSE) & (length(cells@neighbors) == 0)) {
        stop(paste("No K-nearest-neighbor graph but recalculate_graph is",
                   "FALSE. Set to TRUE or run Seurat::FindNeighbors()."))
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
#' `cells' containing the labels of each cell.
#' @param label_1 String containing the name of one of the labels.
#' @param label_2 String containing the name of the other label.
#' @param reduced_dim String containing the name of the dimensionality
#' reduction to use.
#' @param n_dims Integer number of dimensions to use if computing graph
#' (optional, default 10).
#' @param nn_model String containing the path to the model's .hdf5 file
#' (optional, default "~/.dawnn/dawnn_nn_model.h5").
#' @param recalculate_graph Boolean whether to recalculate the KNN graph. If
#' FALSE, then the one stored in the `cells` object will be used (optional,
#' default = TRUE).
#' @param alpha Numeric target false discovery rate supplied to the
#' Benjamini–Yekutieli procedure (optional, default 0.1, i.e. 10%).
#' @param verbosity Integer how much output to print. 0: silent; 1: normal
#' output; 2: display messages from predict() function.
#' @param seed Integer random seed (optional, default 123).
#' @return Seurat dataset `cells' with added metadata: dawnn_scores (output of
#' Dawnn's model for each cell); dawnn_lfc (estimated log2-fold change in the
#' neighbourhood of each cell); dawnn_p_vals (p-values associated with the
#' hypothesis tests for whether a cell is in a region of differential
#' abundance; dawnn_da_verdict (Boolean output of Dawnn indicating whether it
#' considers a cell to be in a region of differential abundance).
#' @examples
#' \dontrun{
#' run_dawnn(cells = dataset, label_names = "condition", nn_model =
#' "my_model.h5", reduced_dim = "pca", n_dims = 50, recalculate_graph = FALSE,
#' alpha = 0.2, verbosity = 0, seed = 42)
#' }
#' @export
run_dawnn <- function(cells, label_names, label_1, label_2, reduced_dim,
                      n_dims = 10, nn_model = "~/.dawnn/dawnn_nn_model.h5",
                      recalculate_graph = TRUE, alpha = 0.1, verbosity = 2,
                      seed = 123) {
    set.seed(seed)

    num_cells <- ncol(cells)
    if (num_cells < 1001) {
        stop(paste0("Dawnn requires at least 1001 cells. Your dataset contains ",
                    num_cells, "."))
    }

    param_check(cells, label_names, label_1, label_2, reduced_dim,
                recalculate_graph)

    if (class(nn_model)[1] == "character") {
        nn_model <- load_model_from_python(nn_model)
    }

    if (recalculate_graph) {
        if (verbosity > 0) {
            message("Finding neighbors.")
        }
        cells <- FindNeighbors(cells, dims = (1:n_dims),
                               return.neighbor = TRUE, k.param = 1001,
                               reduction = reduced_dim)
    }

    if (verbosity > 0) {
        message("Generating neighbor labels.")
    }
    neighbor_labels <- generate_neighbor_labels(cells,
                                                label_names = label_names,
                                                label_1 = label_1,
                                                verbose = verbosity > 0)

    if (verbosity > 0) {
        message("Generating scores.")
    }
    scores <- nn_model$predict(neighbor_labels,
                               verbose = ifelse(verbosity == 2, 1, 0))
    cells$dawnn_scores <- scores
    cells$dawnn_lfc <- log2(scores / (1 - scores))

    if (verbosity > 0) {
        message("Generating null distribution.")
    }
    null_dist <- generate_null_dist(cells, nn_model, label_names, label_1,
                                    label_2, verbosity = verbosity)

    if (verbosity > 0) {
        message("Generating p-values.")
    }
    p_vals <- generate_p_vals(scores, null_dist)
    cells$dawnn_p_vals <- p_vals

    if (verbosity > 0) {
        message("Determining significance.")
    }
    cells$dawnn_da_verdict <- determine_if_region_da(p_vals, scores, null_dist,
                                                     alpha = 0.1)

    return(cells)
}
