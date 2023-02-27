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
#' @param k Integer number of neighbors to use (should be left as 1000 unless
#' you have a very good reason) (optional, default 1000).
#' @param verbose Boolean verbosity (optional, default = TRUE).
#' @param label_names String containing the name of the meta.data slot in
#' `cells' containing the labels of each cell (optional, default =
#' "synth_labels").
#' @return A data frame containing the labels of the k nearest neighbors of
#' each cell.
#' @examples
#' \dontrun{
#' generate_neighbor_labels(cell_object, "pca")
#' }
generate_neighbor_labels <- function(cells, k = 1000, verbose = TRUE,
                                     label_names = "synth_labels") {

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
    nhbor_labels_binary_df <- nhbor_labels_df == "Condition1"
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

    tensorflow <- import("tensorflow")

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
#' @param enforce_05 Boolean whether to simulate an equal distribution of
#' Condition1 and Condition2 labels.
#' @param verbosity Integer how much output to print. 0: silent; 1: normal
#' output; 2: display messages from predict() function.
#' @return A vector containing a null distribution of Dawnn's model outputs for
#' shuffled sample labels.
#' @examples
#' \dontrun{
#' generate_null_dist(cells = cell_object, model = nn_model, label_names =
#' "synth_labels", enforce_05 = TRUE, verbosity = 1)
#' }
generate_null_dist <- function(cells, model, label_names, enforce_05,
                               verbosity) {
    null_dist <- c()
    for (i in 1:3) {
        if (enforce_05) {
            num_cells <- ncol(cells)
            labels <- c(rep("Condition1", round(num_cells / 2)),
                        rep("Condition2", num_cells - round(num_cells / 2)))
            cells$shuff_labels <- sample(labels)
        } else {
            cells$shuff_labels <- sample(cells@meta.data[[label_names]])
        }
        shuff_nbor_labs <- generate_neighbor_labels(cells,
                                                    label_names = "shuff_labels",
                                                    verbose = verbosity > 0)
        shuff_scores <- predict(model, shuff_nbor_labs,
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
#' @param two_sided Boolean whether to use 1-(a calculated p-value) for a score
#' greater than the mode of the beta distribution fitted to the null
#' distribution (optional, default TRUE).
#' @return Numeric vector containing a p-value for each cell, i.e. the
#' probability of observing at least such an extreme score for a cell given the
#' beta distribution fitted to the null distribution of scores.
#' @examples
#' \dontrun{
#' generate_p_vals(scores = score_vect, null_dist = null_scores, two_sided
#' = TRUE)
#' }
generate_p_vals <- function(scores, null_dist, two_sided = TRUE) {
    null_dist_est_params <- beta_method_of_moments(null_dist)
    null_alpha <- null_dist_est_params$alpha
    null_beta <- null_dist_est_params$beta
    null_mode <- (null_alpha - 1) / (null_alpha + null_beta - 2)

    p_vals <- c()

    for (score in scores) {
        if (two_sided) {
            if (score <= null_mode) {
                p_vals <- c(p_vals, pbeta(score, null_alpha, null_beta))
            } else {
                p_vals <- c(p_vals, 1 - pbeta(score, null_alpha, null_beta))
            }
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
#' Benjamini–Yekutieli procedure (optional, default 0.1, i.e. 10%).
#' @param assume_independence Boolean whether to assume that the score
#' for each cell is independent. Intended for testing purposes, do not change
#' unless you have good reason (optional, default FALSE)
#' @param method Whether to determine differential abundance using p-values
#' from the fitted beta distribution or seeing which cells have scores more
#' extreme than any in the null distribution. Intended for testing purposes, do
#' not change unless you have good reason (optional, default "beta")
#' @return Boolean vector containing Dawnn's verdict for each cell.
#' @examples
#' \dontrun{
#' determine_if_region_da(p_vals = p_value_vector, null_dist = null_scores,
#' alpha = 0.2, assume_independence = FALSE, method = "beta")
#' }
determine_if_region_da <- function(p_vals, scores, null_dist, alpha = 0.1,
                                       assume_independence = FALSE,
                                       method = "beta") {
    if (method == "beta") {
        num_cells <- length(p_vals)
        if (assume_independence == FALSE) {
            c <- 0
            for (k in 1:num_cells) {
                c <- (c + (1 / k))
            }
        }
        da_verdict <- rep(FALSE, num_cells)
        j <- 1
        for (i in order(p_vals)) {
            if (assume_independence == FALSE) {
                # This is in fact the "Benjamini–Yekutieli procedure", which
                # allows for arbitrary dependence assumptions. We can remove
                # the "c" if we assume that all tests are independent.
                cutoff <- (j * alpha) / (num_cells * c)
            } else {
                cutoff <- (j * alpha) / num_cells
            }

            if (p_vals[i] <= cutoff) {
                da_verdict[i] <- TRUE
            } else {
                break
            }
            j <- j + 1
        }
    } else if (method == "perturbation") {
        da_verdict <- scores > max(null_dist)
    } else {
        stop(paste("Unknown DA-calling method:", method))
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
#' @param model_url String url from which to download the model (optional,
#' default to hard-coded URL).
#' @param model_file_path String path at which to save the downloaded model.
#' @return Absolute path to the downloaded model.
#' @examples
#' \dontrun{
#' model_path <- download_model(model_url = "http://example.com/model.h5")
#' cells <- run_dawnn(cells, nn_model = model_path, [...])
#' }
#' @export
download_model <- function(model_url = "http://example.com/hard/coded/path",
                           model_file_path = "dawnn_nn_model.h5") {
    system(paste0("wget -O ", model_file_path, " '", model_url, "'"))

    return(normalizePath(model_file_path))
}


#' Identify which cells are in regions of differential abundance using Dawnn.
#'
#' @description `run_dawnn()` is the main function used to run Dawnn. It takes
#' a Seurat dataset and identifies which cells are in regions of differential
#' abundance.
#'
#' @param cells Seurat object containing the dataset.
#' @param label_names String containing the name of the meta.data slot in
#' `cells' containing the labels of each cell.
#' @param reduced_dim String containing the name of the dimensionality
#' reduction to use.
#' @param nn_model String containing the path to the model's .hdf5 file.
#' @param recalculate_graph Boolean whether to recalculate the KNN graph. If
#' FALSE, then the one stored in the `cells` object will be used (optional,
#' default = TRUE).
#' @param two_sided Boolean whether to use 1-(a calculated p-value) for a score
#' greater than the mode of the beta distribution fitted to the null
#' distribution (optional, default TRUE).
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
#' "my_model.h5", reduced_dim = "pca", recalculate_graph = FALSE, two_sided =
#' FALSE, alpha = 0.2, versboity = 0, seed = 42)
#' }
#' @export
run_dawnn <- function(cells, label_names, reduced_dim,
                      nn_model = "final_model_dawnn.h5",
                      recalculate_graph = TRUE, two_sided = TRUE,
                      alpha = 0.1, verbosity = 2, seed = 123) {
    set.seed(seed)

    if (class(nn_model)[1] == "character") {
        nn_model <- load_model_from_python(nn_model)
    }

    if (recalculate_graph) {
        if (verbosity > 0) {
            message("Finding neighbors.")
        }
        cells <- FindNeighbors(cells, dims = 1:50, return.neighbor = TRUE,
                               k.param = 1001, reduction = reduced_dim)
    }

    if (verbosity > 0) {
        message("Generating neighbor labels.")
    }
    neighbor_labels <- generate_neighbor_labels(cells,
                                                label_names = label_names,
                                                verbose = verbosity > 0)

    if (verbosity > 0) {
        message("Generating scores.")
    }
    scores <- predict(nn_model, neighbor_labels,
                      verbose = ifelse(verbosity == 2, 1, 0))
    cells$dawnn_scores <- scores
    cells$dawnn_lfc <- log2(scores / (1 - scores))

    if (verbosity > 0) {
        message("Generating null distribution.")
    }
    null_dist <- generate_null_dist(cells, nn_model, label_names,
                                    enforce_05 = TRUE,
                                    verbosity = verbosity)

    if (verbosity > 0) {
        message("Generating p-values.")
    }
    p_vals <- generate_p_vals(scores, null_dist, two_sided = two_sided)
    cells$dawnn_p_vals <- p_vals

    if (verbosity > 0) {
        message("Determining significance.")
    }
    cells$dawnn_da_verdict <- determine_if_region_da(p_vals, scores, null_dist,
                                                     alpha = 0.1,
                                                     assume_independence = FALSE,
                                                     method = "beta")

    return(cells)
}
