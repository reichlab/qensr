context("df_to_array")
library(qens)
library(dplyr)
library(tidyr)

test_that("df_to_array works, none missing", {
    forecast_df <- dplyr::bind_rows(
        data.frame(
            id1 = c("a", "a", "a", "a", "a", "b", "b", "b"),
            id2 = c("c", "c", "c", "d", "d", "c", "d", "e"),
            team = "ta",
            model = rep("m1", 8),
            q_prob = c(0.025, 0.5, 0.975, 0.5, 0.975, 0.5, 0.5, 0.5),
            q_val = c(123, 234, 345, 456, 567, 678, 789, 890),
            stringsAsFactors = FALSE
        ),
        data.frame(
            id1 = c("a", "a", "b", "b"),
            id2 = c("c", "d", "c", "d"),
            team = "ta",
            model = rep("m2", 4),
            q_prob = c(0.5, 0.5, 0.5, 0.5),
            q_val = c(987, 876, 765, 654),
            stringsAsFactors = FALSE
        ),
        tidyr::expand_grid(
            id1 = c("a", "b"),
            id2 = c("c", "d", "e"),
            team = "tb",
            model = "m1",
            q_prob = c(0.025, 0.5, 0.975)
        ) %>%
            dplyr::mutate(q_val = rnorm(2 * 3 * 3))
    )

    actual <- qens:::df_to_array(forecast_df,
                                 model_id_vars = c("team", "model"),
                                 task_id_vars = c("id1", "id2"),
                                 tau_var = "q_prob", q_var = "q_val")

    expect_identical(actual$predictions, expected_predictions)
    expect_identical(actual$y, expected_predictions)
    expect_identical(actual$task_id, expected_predictions)
    expect_identical(actual$tau, expected_predictions)
    expect_identical(actual$model_id, expected_predictions)
    expected_row_index <- expand.grid(
        id1 = c("a", "b"),
        id2 = c("c", "d", "e"),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )

    expected_col_index <- expand.grid(
      q_prob = as.character(c(0.025, 0.5, 0.975)),
      model = c("m1", "m2"),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

  expected_forecast_matrix <- matrix(NA_real_,
    nrow = nrow(expected_row_index), ncol = nrow(expected_col_index))
  expected_forecast_matrix[1, 1] <- 123.0
  expected_forecast_matrix[1, 2] <- 234.0
  expected_forecast_matrix[1, 3] <- 345.0
  expected_forecast_matrix[3, 2] <- 456.0
  expected_forecast_matrix[3, 3] <- 567.0
  expected_forecast_matrix[2, 2] <- 678.0
  expected_forecast_matrix[4, 2] <- 789.0
  expected_forecast_matrix[6, 2] <- 890.0
  expected_forecast_matrix[1, 5] <- 987.0
  expected_forecast_matrix[3, 5] <- 876.0
  expected_forecast_matrix[2, 5] <- 765.0
  expected_forecast_matrix[4, 5] <- 654.0
  attributes(expected_forecast_matrix) <- c(
    attributes(expected_forecast_matrix),
    list(
      row_index = expected_row_index,
      col_index = expected_col_index,
      model_col = "model",
      quantile_name_col = "q_prob",
      quantile_value_col = "q_val"
    )
  )
  class(expected_forecast_matrix) <- c("QuantileForecastMatrix", "matrix")

  actual <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = "model",
    id_cols = c("id1", "id2"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  expect_identical(actual, expected_forecast_matrix)
})


test_that("new_QuantileForecastMatrix_from_df throws error with duplicated forecasts", {
  forecast_df <- dplyr::bind_rows(
    data.frame(
      id1 = c("a", "a", "a", "a", "a", "b", "b", "b"),
      id2 = c("c", "c", "c", "d", "d", "c", "d", "e"),
      model = rep("m1", 8),
      q_prob = c(0.025, 0.5, 0.975, 0.5, 0.975, 0.5, 0.5, 0.5),
      q_val = c(123, 234, 345, 456, 567, 678, 789, 890),
      stringsAsFactors = FALSE
    ),
    data.frame(
      id1 = c("a", "a", "b", "b"),
      id2 = c("c", "d", "c", "d"),
      model = rep("m2", 4),
      q_prob = c(0.5, 0.5, 0.5, 0.5),
      q_val = c(987, 876, 765, 654),
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    new_QuantileForecastMatrix_from_df(
      forecast_df,
      model_col = "model",
      id_cols =  "id1",
      quantile_name_col = "q_prob",
      quantile_value_col = "q_val"
    )
  )
})
