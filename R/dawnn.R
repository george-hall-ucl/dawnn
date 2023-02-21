library(reticulate)
library(tensorflow)
library(keras)

library(Seurat)


beta_method_of_moments <- function(data) {
    sample_mean <- mean(data)
    sample_var <- var(data)
    common_factor <- ((sample_mean * (1 - sample_mean)) / sample_var) - 1

    alpha = sample_mean * common_factor
    beta = (1 - sample_mean) * common_factor

    return(list(alpha = alpha, beta = beta))
}


generate_neighbor_labels <- function(cells, reduced_dim, k = 1000,
                                     verbose = TRUE,
                                     label_names = "synth_labels",
                                     recalculate_graph = TRUE) {

    if (recalculate_graph) {
        if (verbose) {message("Finding neighbors.")}
        cells <- FindNeighbors(cells, dims = 1:50, return.neighbor = TRUE,
                               k.param = k+1, reduction = reduced_dim)
    }

    if (verbose) {message("Creating adjacency matrix.")}
    graph_name <- names(cells@neighbors)[1]
    if (length(names(cells@neighbors)) > 1) {
        message(paste("Multiple available graph names. Choosing the first one:",
                      graph_name))
    }
    nhbor_labels_mtx <- apply(cells@neighbors[[graph_name]]@nn.idx, 1,
                              function(x) {cells@meta.data[[label_names]][x][-1]})
    nhbor_labels_df <- data.frame(nhbor_labels_mtx)
    nhbor_labels_binary_df <- nhbor_labels_df == "Condition1"
    nhbor_labels_binary_mtx <- apply(nhbor_labels_binary_df, 1, as.numeric)

    return(nhbor_labels_binary_mtx)
}


load_model_from_python <- function(model_path) {
    # Need to have tensorflow installed in the reticulate environment. Check
    # whether it is installed:
    if (py_module_available("tensorflow") == FALSE) {
        # I found it best to do this using conda.
        # conda_env_name <- "r-reticulate"
        # conda_create(conda_env_name)
        # conda_install(conda_env_name, "tensorflow")
        error("Tensorflow not installed in reticulate environment. Please install following https://rstudio.github.io/reticulate/articles/python_packages.html.")
    }

    tensorflow <- import("tensorflow")

    # load model trained with Python
    model <- load_model_hdf5(model_path)

    return(model)
}


generate_null_dist <- function(cells, reduced_dim, model, label_names, enforce_05, verbosity) {
    null_dist <- c()
    for (i in 1:3) {
        if (enforce_05) {
            num_cells <- length(cells@meta.data[[label_names]])
            labels <- c(rep("Condition1", round(num_cells/2)),
                        rep("Condition2", num_cells - round(num_cells/2)))
            cells@meta.data$shuffled_labels <- sample(labels)
        } else {
            cells@meta.data$shuffled_labels <- sample(cells@meta.data[[label_names]])
        }
        shuffled_neighbor_labels <- generate_neighbor_labels(cells, reduced_dim,
                                                             label_names = "shuffled_labels",
                                                             recalculate_graph = FALSE,
                                                             verbose = verbosity > 0)
        shuffled_scores <- model %>% predict(shuffled_neighbor_labels,
                                             verbose = ifelse(verbosity == 2, 1, 0))
        null_dist <- c(null_dist, shuffled_scores)
    }

    return(null_dist)
}


generate_p_vals_pc1 <- function(scores, null_dist, two_sided = TRUE) {
    null_MoM_est <- beta_method_of_moments(null_dist)
    null_MoM_alpha <- null_MoM_est$alpha
    null_MoM_beta <- null_MoM_est$beta
    null_MoM_mode <- (null_MoM_alpha - 1) / (null_MoM_alpha + null_MoM_beta - 2)

    p_vals <- c()

    for (score in scores) {
        if (two_sided) {
            if (score <= null_MoM_mode) {
                p_vals <- c(p_vals, pbeta(score, null_MoM_alpha, null_MoM_beta))
            } else {
                p_vals <- c(p_vals, 1 - pbeta(score, null_MoM_alpha, null_MoM_beta))
            }
        } else {
            p_vals <- c(p_vals, 1 - pbeta(score, null_MoM_alpha, null_MoM_beta))
        }
    }

    return(p_vals)
}

determine_if_region_da_pc1 <- function(p_vals, scores, null_dist, alpha = 0.1,
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
                # This is in fact the "Benjamini–Yekutieli procedure", which allows
                # for arbitrary dependence assumptions. We can remove the
                # "c" if we assume that all tests are independent.
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
        error(paste("Unknown DA-calling method:", method))
    }

    return(da_verdict)
}

run_dawnn <- function(cells, label_names, nn_model = "final_model_dawnn.h5",
                      reduced_dim = NULL, recalculate_graph = TRUE,
                      two_sided = TRUE, alpha = 0.1, verbosity = 2) {

    if (class(nn_model)[1] == "character") {
        nn_model <- load_model_from_python(nn_model)
    }

    if (verbosity > 0) {message("Generating neighbor labels.")}
    neighbor_labels <- generate_neighbor_labels(cells, reduced_dim,
                                                label_names = label_names,
                                                recalculate_graph = recalculate_graph,
                                                verbose = verbosity > 0)

    if (verbosity > 0) {message("Generating scores.")}
    scores <- nn_model %>% predict(neighbor_labels,
                                   verbose = ifelse(verbosity == 2, 1, 0))
    cells@meta.data$dawnn_scores <- scores
    cells@meta.data$dawnn_lfc <- log2(scores/(1-scores))

    if (verbosity > 0) {message("Generating null distribution.")}
    null_dist <- generate_null_dist(cells, reduced_dim, nn_model, label_names, enforce_05 = T,
                                    verbosity = verbosity)

    if (verbosity > 0) {message("Generating p-values.")}
    p_vals <- generate_p_vals_pc1(scores, null_dist, two_sided = two_sided)
    cells@meta.data$dawnn_p_vals <- p_vals

    if (verbosity > 0) {message("Determining significance.")}
    cells@meta.data$dawnn_da_verdict <- determine_if_region_da_pc1(p_vals, scores, null_dist, alpha = 0.1, assume_independence = FALSE, method = "beta")

    return(cells)
}

