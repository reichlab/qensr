#' Create a qens object representing a quantile ensemble model fit
#'
#' @param model_id_vars `character` vector naming columns of `predictions` that
#'   identify prediction models.
#' @param task_id_vars an optional `character` vector naming the columns of
#'   `predictions` that correspond to task id variables. The default is `NULL`,
#'   in which case all columns in `predictions` other than those specified for
#'   `model_id_vars, `tau_var`, and `q_var` will be used as task id variables.
#' @param tau_var string naming a column of `predictions` that
#'   identifies the probability level of quantile forecasts. The default is
#'   `NULL`, in which case `"quantile"` is used if `predictions` contains a
#'   column of that name, or otherwise `"output_id"` is used if `predictions`
#'   contains a column of that name; these are common naming conventions in
#'   infectious disease forecast hubs. If `tau_var` is not specified and
#'   `predictions` does not contain columns named `"quantile"` or `"output_id"`,
#'   an error is thrown.
#' @param q_var string naming a column of `predictions` that has values of
#'   predictive quantiles.
#' @param combine_method string specifying method for ensembling quantiles;
#'   "mean" or "median"
#' @param weighted boolean indicating whether model weighting should be done.
#'   If `FALSE`, all models are given equal weight. If `TRUE`, model weights are
#'   estimated.
#' @param tau_groups Vector of group labels for quantiles, having the same
#'   length as the number of distinct quantile levels in the `tau_var` column of
#'   `predictions`. The ensemble weights are shared across any quantile levels
#'   with the same label. The default specifies that a common set of weights
#'   should be used across all levels.
#' @param model_id data frame of model identifiers corresponding to weights
#' @param tau numeric vector of quantile levels
#' @param parameters named list of model parameters, for internal use by python
#'   qens module
#' @param weights numeric array of weights, with shape
#'   (number of quantile levels, number of models)
#' @param loss_trace numeric vector of loss trace per iteration of estimation
#'
#' @return S3 object with class `<qens>`
#'
#' @export
new_qens <- function(model_id_vars, task_id_vars, tau_var, q_var,
                     combine_method, weighted, tau_groups, model_id, tau,
                     parameters, weights, loss_trace) {
    qens_obj <- structure(
        list(
            model_id_vars = model_id_vars,
            task_id_vars = task_id_vars,
            tau_var = tau_var,
            q_var = q_var,
            combine_method = combine_method,
            weighted = weighted,
            tau_groups = tau_groups,
            model_id = model_id,
            tau = tau,
            parameters = parameters,
            weights = weights,
            loss_trace = loss_trace
        ),
        class = "qens"
    )

#    validate_qens(qra_fit)

    return(qens_obj)
}


#' Instantiate a new Python object representing a quantile ensemble fit from an
#' R `qens` object.
#' 
#' @param qens_obj An S3 object of class `<qens>`
#' 
#' @return A pointer to a python `qens` object, for use with reticulate
new_qenspy_from_qens <- function(qens_obj) {
    if (qens_obj$combine_method == "mean") {
        qenspy_obj <- qenspy$MeanQEns(M = nrow(qens_obj$model_id),
                                      tau = qens_obj$tau,
                                      tau_groups = qens_obj$tau_groups,
                                      init_method = "equal")
    } else if (qens_obj$combine_method == "median") {
        qenspy_obj <- qenspy$MedianQEns(M = nrow(model_id),
                                        tau = qens_obj$tau,
                                        tau_groups = qens_obj$tau_groups,
                                        init_method = "equal")
    }

    for (p_name in names(qens_obj$parameters)) {
        qenspy_obj$set_param(p_name, qens_obj$parameters[[p_name]])
    }

    qenspy_obj$loss_trace <- qens_obj$loss_trace

    return(qenspy_obj)
}


#' validate that the column names of `df_sub` and `df_sup` are the same,
#' and that all rows of df_sub appear in df_sup
#'
#' @param df_sub a data frame that is expected to be a subset
#' @param df_sup a dataframe that is expected to be a superset
#' 
#' @return TRUE if `df_sub` and `df_sup` have the same column names and all rows
#'   of df_sub are in df_sup, FALSE otherwise
validate_df_subseteq <- function(df_sub, df_sup) {
    if (!identical(sort(colnames(df_sub)), sort(colnames(df_sup)))) {
        return(FALSE)
    }

    if (nrow(dplyr::anti_join(df_sub, df_sup, by = colnames(df_sub))) > 0) {
        return(FALSE)
    }

    return(TRUE)
}


#' Convert data frames with predictions and observed data to arrays.
#' It's OK not to include the data.
#'
#' @inheritParams qens
#'
#' @details This function expands across missing combinations of task id, model,
#'   and quantile level, filling in missing entries in the predictions with
#'   `NA_real_`. It also subsets the predictions and the observed data, only
#'
#' @return named list with entries:
#'   - `predictions`: array with shape (N, K, M) where N is the number of
#'     prediction tasks, K is the number of quantile levels, and M is the
#'     number of models.
#'   - `y`: array with observed data corresponding to the first dimension of
#'     predictions
#'   - `task_id`: data frame with N rows containing the task id variables
#'     corresponding to the first axis of `predictions` and the entries of `y`.
#'     Column names are those specified by `task_id_vars`
#'   - `tau`: numeric vector of length K with quantile levels corresponding to
#'     the second axis of `predictions`.
#'   - `model_id`: data frame with M rows containing the model identifiers
#'     corresponding to the third axis of `predictions`. Column names are those
#'     specified by `model_id_vars`
#' 
#' @importFrom dplyr distinct left_join pull
#' @importFrom tidyr pivot_wider all_of
#' @importFrom purrr map reduce
df_to_array <- function(predictions, y = NULL,
                        model_id_vars, task_id_vars, tau_var, q_var,
                        model_id = NULL, tau = NULL) {
    pred_model_id <- dplyr::distinct(predictions[model_id_vars])
    if (is.null(model_id)) {
        model_id <- pred_model_id
    } else {
        # validate that all model ids that appear in predictions are in the
        # provided model_id
        if (!validate_df_subseteq(pred_model_id, model_id)) {
            stop("If `model_id` is provided, it must contain model ids for all",
                 " models in `predictions`.")
        }
    }

    task_id <- dplyr::distinct(predictions[task_id_vars])

    pred_tau <- sort(unique(predictions[[tau_var]]))
    if (is.null(tau)) {
        tau <- pred_tau
    } else {
        # validate that all taus that appear in predictions are in the
        # provided tau vector
        if (!all(pred_tau %in% tau)) {
            stop("If `tau` is provided, it must contain all tau levels in",
                 " `predictions`.")
        }
    }
    tau_chr <- as.character(tau)

    N <- nrow(task_id)
    K <- length(tau)
    M <- nrow(model_id)

    # check for duplicated combinations of model, task, and tau, indicating
    # an error in data collection or a missing task id or model variable
    id_vars <- c(model_id_vars, task_id_vars, tau_var)
    if (any(duplicated(predictions[, id_vars]))) {
        stop("predictions contained multiple rows for the same combination of",
             " the model id, task id, and tau variables.")
    }

    wide_predictions <- predictions %>%
        tidyr::pivot_wider(names_from = tidyr::all_of(tau_var),
                           values_from = tidyr::all_of(q_var))

    predictions_arr_by_model <- purrr::map(
        seq_len(M),
        function(m) {
            # get predictions for model m with a column per quantile level
            m_preds <- model_id[m, , drop = FALSE] %>%
                # left join predictions to model id selects model m
                dplyr::left_join(predictions, by = model_id_vars,
                                 multiple = "all") %>%
                # pivot quantiles to columns
                tidyr::pivot_wider(names_from = tidyr::all_of(tau_var),
                                   values_from = tidyr::all_of(q_var))

            # left join to task id ensures there's a row for every task id and
            # the order matches task id order
            m_preds <- dplyr::left_join(task_id, m_preds, by = task_id_vars,
                                        multiple = "all")

            # augment with columns for missing taus
            tau_miss <- tau_chr[!(tau_chr %in% colnames(m_preds))]
            for (t in tau_miss) {
                m_preds[[t]] <- NA_real_
            }

            # return predictive quantiles as a matrix
            return(as.matrix(m_preds[as.character(tau)]))
        }
    )

    # column bind per-model arrays to matrix with shape (N, K*M)
    # then, reshape to the desired 3d shape
    predictions_arr <- purrr::reduce(predictions_arr_by_model, cbind)
    dim(predictions_arr) <- c(N, K, M)

    if (!is.null(y)) {
        # get data values in the same order as the predictions
        join_vars <- task_id_vars[task_id_vars %in% colnames(y)]
        y_arr <- task_id %>%
            dplyr::left_join(y, by = join_vars,
                             multiple = "all") %>%
            dplyr::pull(value)

        # drop any rows for which observed data are not available
        y_miss <- is.na(y_arr)
        task_id <- task_id[!y_miss, , drop = FALSE]
        row.names(task_id) <- NULL
        predictions_arr <- predictions_arr[!y_miss, , , drop = FALSE]
        y_arr <- y_arr[!y_miss]
    } else {
        y_arr <- NULL
    }

    return(list(
        predictions = predictions_arr,
        y = y_arr,
        task_id = task_id,
        tau = tau,
        model_id = model_id
    ))
}



#' Fit a quantile ensemble model, estimating model parameters based on
#' training set predictions and corresponding observed data.
#'
#' @param predictions data frame with training set predictions to use for
#'   estimating model weights.
#' @param y data frame with observed responses for training set predictions
#' @param model_id_vars `character` vector naming columns of `predictions` that
#'   identify prediction models.
#' @param task_id_vars an optional `character` vector naming the columns of
#'   `predictions` that correspond to task id variables. The default is `NULL`,
#'   in which case all columns in `predictions` other than those specified for
#'   `model_id_vars, `tau_var`, and `q_var` will be used as task id variables.
#' @param tau_var string naming a column of `predictions` that
#'   identifies the probability level of quantile forecasts. The default is
#'   `NULL`, in which case `"quantile"` is used if `predictions` contains a
#'   column of that name, or otherwise `"output_id"` is used if `predictions`
#'   contains a column of that name; these are common naming conventions in
#'   infectious disease forecast hubs. If `tau_var` is not specified and
#'   `predictions` does not contain columns named `"quantile"` or `"output_id"`,
#'   an error is thrown.
#' @param q_var string naming a column of `predictions` that has values of
#'   predictive quantiles.
#' @param combine_method string specifying method for ensembling quantiles;
#'   "mean" or "median"
#' @param weighted boolean indicating whether model weighting should be done.
#'   If `FALSE`, all models are given equal weight. If `TRUE`, model weights are
#'   estimated.
#' @param tau_groups Vector of group labels for quantiles, having the same
#'   length as the number of distinct quantile levels in the `tau_var` column of
#'   `predictions`. The ensemble weights are shared across any quantile levels
#'   with the same label. The default specifies that a common set of weights
#'   should be used across all levels.
#' @param init_method string specifying initialization method for parameters.
#'   Choices are "xavier" for random Xavier initialization or "equal" for equal
#'   weights. Ignored if `weighted` is `FALSE`.
#' @param optim_method string specifying optimization method; options are "adam"
#'   and "sgd"
#' @param num_iter integer number of iterations for optimization
#' @param learning_rate numeric learning rate for optimization
#' @param verbose boolean indicating whether to print output during parameter
#'   estimation
#'
#' @return S3 object of class `<qens>`
#'
#' @export
qens <- function(predictions, y,
                 model_id_vars = "model",
                 task_id_vars = NULL,
                 tau_var = NULL,
                 q_var = "value",
                 combine_method = "mean",
                 weighted = FALSE,
                 tau_groups = rep(1, length(unique(predictions[[tau_var]]))),
                 init_method = NULL,
                 optim_method = "adam",
                 num_iter = 1000,
                 learning_rate = 0.1,
                 verbose = FALSE,
                 save_frequency = NULL,
                 save_filename = NULL) {
    # validate arguments
    if (!all(model_id_vars %in% colnames(predictions))) {
        stop("All `model_id_vars` must be columns of `predictions`.")
    }
    if (!all(task_id_vars %in% colnames(predictions))) {
        stop("All `task_id_vars` must be columns of `predictions`.")
    }
    if (!all(tau_var %in% colnames(predictions))) {
        stop("`tau_var` must be a column of `predictions`.")
    }
    if (!all(q_var %in% colnames(predictions))) {
        stop("`q_var` must be a column of `predictions`.")
    }
    weighted <- as.logical(weighted)
    
    if (is.null(init_method)) {
        if (weighted) init_method <- "xavier" else init_method <- "equal"
    }

    tau_groups <- as.integer(factor(tau_groups)) - 1

    # reformat training set predictive quantiles from component models as 3d
    # array in format required for qenspy
    c(predictions, y, task_id, tau, model_id) %<-%
        df_to_array(predictions = predictions, y = y,
                    model_id_vars = model_id_vars,
                    task_id_vars = task_id_vars,
                    tau_var = tau_var, q_var = q_var)

    # estimate ensemble parameters
    if (combine_method == "mean") {
        qenspy_model <- qenspy$MeanQEns(M = nrow(model_id), tau = tau,
                                        tau_groups = tau_groups,
                                        init_method = init_method)
    } else if (combine_method == "median") {
        qenspy_model <- qenspy$MedianQEns(M = nrow(model_id), tau = tau,
                                          tau_groups = tau_groups,
                                          init_method = init_method)
    }

    if (weighted) {
        qenspy_model$fit(
            y,
            predictions,
            optim_method = optim_method,
            num_iter = as.integer(num_iter),
            learning_rate = learning_rate,
            verbose = verbose,
            save_frequency = save_frequency,
            save_path = save_filename)
    }

    # Create R object representing the model fit
    qens_obj <- new_qens(
        model_id_vars = model_id_vars,
        task_id_vars = task_id_vars,
        tau_var = tau_var,
        q_var = q_var,
        combine_method = combine_method,
        weighted = weighted,
        tau_groups = tau_groups,
        model_id = model_id,
        tau = tau,
        parameters = purrr::map(
            names(qenspy_model$parameters),
            function(p_name) qenspy_model$parameters[[p_name]]$numpy()) %>%
            `names<-`(names(qenspy_model$parameters)),
        weights = qenspy_model$w$numpy(),
        loss_trace = qenspy_model$loss_trace
    )

    return(qens_obj)
}



#' Predict based on a quantile ensemble fit
#'
#' @param qens object of class qens
#' @param predictions data frame with component model predictions to combine
#' @param sort_quantiles logical; if TRUE, the predictive quantiles are sorted
#'   in order of the quantile level.  Otherwise, the raw predictive quantiles
#'   are returned.
#'
#' @return data frame with ensemble quantile forecasts
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom dplyr group_by ungroup arrange mutate across
#' @importFrom rlang sym
#'
#' @export
predict.qens <- function(qens_obj, predictions, sort_quantiles = TRUE) {
    # convert predictions data frame to array, providing the values of id
    # variables that came from the training set to ensure consistency with
    # quantile levels and model ids
    c(predictions, y, task_id, tau, model_id) %<-%
        df_to_array(predictions = predictions,
                    model_id_vars = qens_obj$model_id_vars,
                    task_id_vars = qens_obj$task_id_vars,
                    tau_var = qens_obj$tau_var,
                    q_var = qens_obj$q_var,
                    model_id = qens_obj$model_id,
                    tau = qens_obj$tau)

    # create python object representing the model fit
    qenspy_obj <- new_qenspy_from_qens(qens_obj)

    # get_predictions; shape is (num_forecast_tasks, num_quantile_levels)
    predictions_raw <- qenspy_obj$predict(q = predictions)

    # convert predictions to a long data frame
    predictions_df <- as.data.frame(predictions_raw$numpy()) %>%
        `colnames<-`(as.character(qens_obj$tau))
    predictions_df <- cbind(task_id, predictions_df) %>%
        tidyr::pivot_longer(cols = tidyselect::all_of(as.character(qens_obj$tau)),
                            names_to = qens_obj$tau_var,
                            values_to = qens_obj$q_var,
                            names_transform = as.numeric)

    # sort predictive quantiles if requested
    if (sort_quantiles) {
        q_var <- rlang::sym(qens_obj$q_var)
        predictions_df <- predictions_df %>%
            dplyr::group_by(dplyr::across(colnames(task_id))) %>%
            dplyr::arrange(!!qens_obj$tau_var, .by_group = TRUE) %>%
            dplyr::mutate(!!qens_obj$q_var := sort({{ q_var }})) %>%
            dplyr::ungroup()
    }

    return(predictions_df)
}
