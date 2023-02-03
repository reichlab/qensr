#' Create a qens object
#'
#' @param param_vec numeric vector of "raw" parameter estimates
#' @param M integer number of models
#' @param model_names name for each model
#' @param tau numeric vector of quantile levels
#' @param tau_grps specification of quantile groups
#' @param combine_method string specifying combination method;
#' either "convex_mean" or "convex_median"
#' @param loss_trace numeric vector of loss trace per iteration of estimation
#'
#' @return qens object
#'
#' @export
new_qens <- function(
    param_vec,
    M,
    model_names = rep("", M),
    quantile_levels,
    quantile_groups,
    combine_method,
    loss_trace = NULL
) {
    qens_obj <- structure(
        list(
            param_vec = param_vec,
            M = M,
            model_names = model_names,
            quantile_levels = quantile_levels,
            quantile_groups = quantile_groups,
            combine_method = combine_method,
            loss_trace = loss_trace
        ),
        class = 'qens'
    )

#    validate_qens(qra_fit)

    return(qens_obj)
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
df_to_array <- function(predictions, y = NULL,
                        model_id_vars, task_id_vars, tau_var, q_var) {
    task_id <- dplyr::distinct(predictions[task_id_vars])
    tau <- sort(unique(predictions[[tau_var]]))
    tau_chr <- as.character(tau)
    model_id <- dplyr::distinct(predictions[model_id_vars])

    N <- nrow(task_id)
    K <- length(tau)
    M <- nrow(model_id)

    wide_predictions <- predictions %>%
        tidyr::pivot_wider(names_from = !!tau_var, values_from = !!q_var)

    predictions_arr_by_model <- purrr::map(
        seq_len(M),
        function(m) {
            # get predictions for model m with a column per quantile level
            m_preds <- model_id[m, ] %>%
                # left join predictions to model id selects model m
                dplyr::left_join(predictions, by = model_id_vars) %>%
                # pivot quantiles to columns
                tidyr::pivot_wider(names_from = tau_var,
                                   values_from = q_var) %>%
                # right join to task id ensures there's a row for every task id
                dplyr::right_join(task_id, by = task_id_vars)

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
        y_arr <- task_id %>%
            dplyr::left_join(y, by = task_id_vars) %>%
            dplyr::pull(value)

        # drop any rows for which observed data are not available
        y_miss <- is.na(y_arr)
        predictions_arr <- predictions_arr[!y_miss, , ]
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
#' @param tau_grps Vector of group labels for quantiles, having the same
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
                 tau_grps = rep(1, length(unique(predictions[[tau_var]]))),
                 init_method = if (weighted) "xavier" else "equal",
                 optim_method = "adam",
                 num_iter = 1000,
                 learning_rate = 0.1,
                 verbose = FALSE,
                 save_frequency = NULL,
                 save_filename = NULL) {
    # validate arguments

    tau_grps <- as.integer(factor(tau_grps)) - 1

    # reformat training set predictive quantiles from component models as 3d
    # array in format required for qenspy
    c(predictions, y, task_id, tau, model_id) %<-%
        df_to_array(predictions = predictions, y = y,
                    model_id_vars = model_id_vars,
                    task_id_vars = task_id_vars,
                    tau_var = tau_var, q_var = q_var)

    # estimate ensemble parameters
    if (combine_method == "mean") {
        qenspy_model <- qenspy$MeanQEns()
    } else if (combine_method == "median") {
        qenspy_model <- qenspy$MedianQEns()
    }

    qenspy_model$fit(
        y,
        predictions,
        as.numeric(tau),
        tau_grps,
        init_method = init_method,
        optim_method = optim_method,
        num_iter = as.integer(num_iter),
        learning_rate = learning_rate,
        verbose = verbose,
        save_frequency = partial_save_frequency,
        save_path = partial_save_filename)

    # Create R object representing the model fit
    qens_obj <- new_qens(
        param_vec = qenspy_model$parameters(),
        model_id = model_id,
        tau = tau,
        tau_grps = tau_grps,
        combine_method = combine_method,
        loss_trace = qenspy_model$loss_trace
    )

    return(qens_obj)
}



#' Predict based on a quantile regression averaging fit
#'
#' @param qens object of class qens
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param sort_quantiles logical; if TRUE, the predictive quantiles are sorted
#' in order of the quantile level.  Otherwise, the raw predictive quantiles are
#' returned.
#'
#' @return data frame with ensemble quantile forecasts
#'
#' @export
predict.qens <- function(qens_obj, qfm, sort_quantiles) {
    # construct sparse matrix representing model weights across quantiles
    ## pull out parameter values
    row_index <- attr(qfm, 'row_index')
    col_index <- attr(qfm, 'col_index')
    num_models <- qens_obj$M

    quantiles <- col_index[[attr(qfm, 'quantile_name_col')]] %>%
        unique() %>%
        sort() %>%
        as.numeric()
    num_quantiles <- length(quantiles)

    qarr <- unclass(qfm)
    dim(qarr) <- c(nrow(qarr), num_quantiles, num_models)

    # create qenspy object of appropriate class
    if (qens_obj$combine_method == "convex_mean") {
        qenspy_model <- qenspy$MeanQEns()
    } else if (qens_obj$combine_method == "convex_median") {
        qenspy_model <- qenspy$MedianQEns()
    } else {
        stop("combine_method must be convex_mean or convex_median")
    }

    # get_predictions
    predictions_raw <- qens_model$predict(
        q = qarr,
        w = qenspy_model$unpack_params(
            qens_obj$param_vec,
            M = num_models,
            tau_groups = qens_obj$quantile_groups
        )
    )
    result_matrix <- predictions_raw$numpy()

    # Create QuantileForecastMatrix with result
    model_col <- attr(qfm, 'model_col')
    new_col_index <- col_index[
        col_index[[model_col]] == col_index[[model_col]][1], ]
    new_col_index[[model_col]] <- 'qra'

    result_qfm <- new_QuantileForecastMatrix(
        qfm = result_matrix,
        row_index = row_index,
        col_index = new_col_index,
        model_col = attr(qfm, 'model_col'),
        quantile_name_col = attr(qfm, 'quantile_name_col'),
        quantile_value_col = attr(qfm, 'quantile_value_col')
    )

    # sort predictive quantiles if requested
    if (sort_quantiles) {
        result_qfm <- sort(result_qfm)
    }

    return(result_qfm)
}
