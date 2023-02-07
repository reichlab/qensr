library(qens)
library(dplyr)
library(tidyr)

test_that("df_to_array works, base case, no y", {
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

    expected_task_id <- data.frame(
        id1 = c("a", "a", "b", "b", "b", "a"),
        id2 = c("c", "d", "c", "d", "e", "e"))

    expected_predictions <- array(NA_real_, dim = c(6, 3, 3))
    expected_predictions[1, 1, 1] <- 123.0
    expected_predictions[1, 2, 1] <- 234.0
    expected_predictions[1, 3, 1] <- 345.0
    expected_predictions[2, 2, 1] <- 456.0
    expected_predictions[2, 3, 1] <- 567.0
    expected_predictions[3, 2, 1] <- 678.0
    expected_predictions[4, 2, 1] <- 789.0
    expected_predictions[5, 2, 1] <- 890.0
    
    expected_predictions[1, 2, 2] <- 987.0
    expected_predictions[2, 2, 2] <- 876.0
    expected_predictions[3, 2, 2] <- 765.0
    expected_predictions[4, 2, 2] <- 654.0
    
    for (i in 1:2) {
        for (j in 1:3) {
            for (k in 1:3) {
                case_ind <- which(expected_task_id$id1 == c("a", "b")[i] &
                                  expected_task_id$id2 == c("c", "d", "e")[j])
                expected_predictions[case_ind, k, 3] <- forecast_df %>%
                    dplyr::filter(team == "tb",
                                  id1 == c("a", "b")[i],
                                  id2 == c("c", "d", "e")[j],
                                  q_prob == c(0.025, 0.5, 0.975)[k]) %>%
                    dplyr::pull(q_val)
            }
        }
    }
    expect_identical(actual$predictions, expected_predictions)
    expect_identical(actual$y, NULL)
    expect_identical(actual$task_id, expected_task_id)
    expect_identical(actual$tau, c(0.025, 0.5, 0.975))
    expect_identical(actual$model_id,
                     data.frame(team = c("ta", "ta", "tb"),
                                model = c("m1", "m2", "m1")))
})




test_that("df_to_array works, none missing, single task id and model id, no y", {
    forecast_df <- dplyr::bind_rows(
        data.frame(
            id1 = c("a", "a", "a", "b", "b", "c", "d", "e"),
            model = rep("m1", 8),
            q_prob = c(0.025, 0.5, 0.975, 0.5, 0.975, 0.5, 0.5, 0.5),
            q_val = c(123, 234, 345, 456, 567, 678, 789, 890),
            stringsAsFactors = FALSE
        ),
        data.frame(
            id1 = c("a", "b", "d", "e"),
            model = rep("m2", 4),
            q_prob = c(0.5, 0.5, 0.5, 0.5),
            q_val = c(987, 876, 765, 654),
            stringsAsFactors = FALSE
        ),
        tidyr::expand_grid(
            id1 = c("a", "b", "c", "d", "e"),
            model = "m3",
            q_prob = c(0.025, 0.5, 0.975)
        ) %>%
            dplyr::mutate(q_val = rnorm(15))
    )

    actual <- qens:::df_to_array(forecast_df,
                                 model_id_vars = "model",
                                 task_id_vars = "id1",
                                 tau_var = "q_prob", q_var = "q_val")

    expected_task_id <- data.frame(
        id1 = c("a", "b", "c", "d", "e"))

    expected_predictions <- array(NA_real_, dim = c(5, 3, 3))
    expected_predictions[1, 1, 1] <- 123.0
    expected_predictions[1, 2, 1] <- 234.0
    expected_predictions[1, 3, 1] <- 345.0
    expected_predictions[2, 2, 1] <- 456.0
    expected_predictions[2, 3, 1] <- 567.0
    expected_predictions[3, 2, 1] <- 678.0
    expected_predictions[4, 2, 1] <- 789.0
    expected_predictions[5, 2, 1] <- 890.0
    
    expected_predictions[1, 2, 2] <- 987.0
    expected_predictions[2, 2, 2] <- 876.0
    expected_predictions[4, 2, 2] <- 765.0
    expected_predictions[5, 2, 2] <- 654.0
    
    for (i in 1:5) {
        for (k in 1:3) {
            expected_predictions[i, k, 3] <- forecast_df %>%
                dplyr::filter(model == "m3",
                              id1 == c("a", "b", "c", "d", "e")[i],
                              q_prob == c(0.025, 0.5, 0.975)[k]) %>%
                dplyr::pull(q_val)
        }
    }
    expect_identical(actual$predictions, expected_predictions)
    expect_identical(actual$y, NULL)
    expect_identical(actual$task_id, expected_task_id)
    expect_identical(actual$tau, c(0.025, 0.5, 0.975))
    expect_identical(actual$model_id,
                     data.frame(model = c("m1", "m2", "m3")))
})



test_that("df_to_array throws error with duplicated forecasts", {
    forecast_df <- dplyr::bind_rows(
        data.frame(
            id1 = c("a", "a", "a", "a", "a", "b", "b", "a"),
            id2 = c("c", "c", "c", "d", "d", "c", "d", "c"),
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
        qens:::df_to_array(forecast_df,
                           model_id_vars = "model",
                           task_id_vars = c("id1", "id2"),
                           tau_var = "q_prob", q_var = "q_val")
    )
})



test_that("df_to_array works, base case, extra y", {
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

    y <- tidyr::expand_grid(
        id1 = c("a", "b", "w"),
        id2 = c("c", "d", "e", "z")
    ) %>%
        dplyr::mutate(value = rnorm(3 * 4))

    actual <- qens:::df_to_array(forecast_df,
                                 y,
                                 model_id_vars = c("team", "model"),
                                 task_id_vars = c("id1", "id2"),
                                 tau_var = "q_prob", q_var = "q_val")

    expected_task_id <- data.frame(
        id1 = c("a", "a", "b", "b", "b", "a"),
        id2 = c("c", "d", "c", "d", "e", "e"))

    expected_predictions <- array(NA_real_, dim = c(6, 3, 3))
    expected_predictions[1, 1, 1] <- 123.0
    expected_predictions[1, 2, 1] <- 234.0
    expected_predictions[1, 3, 1] <- 345.0
    expected_predictions[2, 2, 1] <- 456.0
    expected_predictions[2, 3, 1] <- 567.0
    expected_predictions[3, 2, 1] <- 678.0
    expected_predictions[4, 2, 1] <- 789.0
    expected_predictions[5, 2, 1] <- 890.0
    
    expected_predictions[1, 2, 2] <- 987.0
    expected_predictions[2, 2, 2] <- 876.0
    expected_predictions[3, 2, 2] <- 765.0
    expected_predictions[4, 2, 2] <- 654.0
    
    for (i in 1:2) {
        for (j in 1:3) {
            for (k in 1:3) {
                case_ind <- which(expected_task_id$id1 == c("a", "b")[i] &
                                  expected_task_id$id2 == c("c", "d", "e")[j])
                expected_predictions[case_ind, k, 3] <- forecast_df %>%
                    dplyr::filter(team == "tb",
                                  id1 == c("a", "b")[i],
                                  id2 == c("c", "d", "e")[j],
                                  q_prob == c(0.025, 0.5, 0.975)[k]) %>%
                    dplyr::pull(q_val)
            }
        }
    }
    
    expected_y <- purrr::pmap_dbl(
        expected_task_id,
        function(id1, id2) {
            y %>% dplyr::filter(id1 == !!id1, id2 == !!id2) %>%
                  dplyr::pull(value)
        }
    )
    
    expect_identical(actual$predictions, expected_predictions)
    expect_identical(actual$y, expected_y)
    expect_identical(actual$task_id, expected_task_id)
    expect_identical(actual$tau, c(0.025, 0.5, 0.975))
    expect_identical(actual$model_id,
                     data.frame(team = c("ta", "ta", "tb"),
                                model = c("m1", "m2", "m1")))
})



test_that("df_to_array works, base case, missing y", {
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

    y <- tidyr::expand_grid(
        id1 = c("a", "b", "w"),
        id2 = c("c", "d", "e", "z")
    ) %>%
        dplyr::mutate(value = rnorm(3 * 4)) %>%
        dplyr::filter(!(id1 == "a" & id2 == "d"))

    actual <- qens:::df_to_array(forecast_df,
                                 y,
                                 model_id_vars = c("team", "model"),
                                 task_id_vars = c("id1", "id2"),
                                 tau_var = "q_prob", q_var = "q_val")

    expected_task_id <- data.frame(
        id1 = c("a", "b", "b", "b", "a"),
        id2 = c("c", "c", "d", "e", "e"))

    expected_predictions <- array(NA_real_, dim = c(6, 3, 3))
    expected_predictions[1, 1, 1] <- 123.0
    expected_predictions[1, 2, 1] <- 234.0
    expected_predictions[1, 3, 1] <- 345.0
    expected_predictions[2, 2, 1] <- 456.0
    expected_predictions[2, 3, 1] <- 567.0
    expected_predictions[3, 2, 1] <- 678.0
    expected_predictions[4, 2, 1] <- 789.0
    expected_predictions[5, 2, 1] <- 890.0
    
    expected_predictions[1, 2, 2] <- 987.0
    expected_predictions[2, 2, 2] <- 876.0
    expected_predictions[3, 2, 2] <- 765.0
    expected_predictions[4, 2, 2] <- 654.0
    
    expected_predictions <- expected_predictions[c(1, 3, 4, 5, 6), , ]
    
    for (i in 1:2) {
        for (j in 1:3) {
            case_ind <- which(expected_task_id$id1 == c("a", "b")[i] &
                              expected_task_id$id2 == c("c", "d", "e")[j])
            if (length(case_ind) > 0) {
                for (k in 1:3) {
                    expected_predictions[case_ind, k, 3] <- forecast_df %>%
                        dplyr::filter(team == "tb",
                                    id1 == c("a", "b")[i],
                                    id2 == c("c", "d", "e")[j],
                                    q_prob == c(0.025, 0.5, 0.975)[k]) %>%
                        dplyr::pull(q_val)
                }
            }
        }
    }
    
    expected_y <- purrr::pmap_dbl(
        expected_task_id,
        function(id1, id2) {
            y %>% dplyr::filter(id1 == !!id1, id2 == !!id2) %>%
                  dplyr::pull(value)
        }
    )
    
    expect_identical(actual$predictions, expected_predictions)
    expect_identical(actual$y, expected_y)
    expect_identical(actual$task_id, expected_task_id)
    expect_identical(actual$tau, c(0.025, 0.5, 0.975))
    expect_identical(actual$model_id,
                     data.frame(team = c("ta", "ta", "tb"),
                                model = c("m1", "m2", "m1")))
})



test_that("df_to_array works, base case, one available y", {
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

    y <- tidyr::expand_grid(
        id1 = c("b", "w"),
        id2 = c("d", "z")
    ) %>%
        dplyr::mutate(value = rnorm(2 * 2))

    actual <- qens:::df_to_array(forecast_df,
                                 y,
                                 model_id_vars = c("team", "model"),
                                 task_id_vars = c("id1", "id2"),
                                 tau_var = "q_prob", q_var = "q_val")

    expected_task_id <- data.frame(
        id1 = c("b"),
        id2 = c("d"))

    expected_predictions <- array(NA_real_, dim = c(6, 3, 3))
    expected_predictions[1, 1, 1] <- 123.0
    expected_predictions[1, 2, 1] <- 234.0
    expected_predictions[1, 3, 1] <- 345.0
    expected_predictions[2, 2, 1] <- 456.0
    expected_predictions[2, 3, 1] <- 567.0
    expected_predictions[3, 2, 1] <- 678.0
    expected_predictions[4, 2, 1] <- 789.0
    expected_predictions[5, 2, 1] <- 890.0
    
    expected_predictions[1, 2, 2] <- 987.0
    expected_predictions[2, 2, 2] <- 876.0
    expected_predictions[3, 2, 2] <- 765.0
    expected_predictions[4, 2, 2] <- 654.0
    
    expected_predictions <- expected_predictions[4, , , drop = FALSE]
    
    for (i in 1:2) {
        for (j in 1:3) {
            case_ind <- which(expected_task_id$id1 == c("a", "b")[i] &
                              expected_task_id$id2 == c("c", "d", "e")[j])
            if (length(case_ind) > 0) {
                for (k in 1:3) {
                    expected_predictions[case_ind, k, 3] <- forecast_df %>%
                        dplyr::filter(team == "tb",
                                    id1 == c("a", "b")[i],
                                    id2 == c("c", "d", "e")[j],
                                    q_prob == c(0.025, 0.5, 0.975)[k]) %>%
                        dplyr::pull(q_val)
                }
            }
        }
    }
    
    expected_y <- purrr::pmap_dbl(
        expected_task_id,
        function(id1, id2) {
            y %>% dplyr::filter(id1 == !!id1, id2 == !!id2) %>%
                  dplyr::pull(value)
        }
    )
    
    expect_identical(actual$predictions, expected_predictions)
    expect_identical(dim(actual$predictions), c(1L, 3L, 3L))
    expect_identical(actual$y, expected_y)
    expect_identical(actual$task_id, expected_task_id)
    expect_identical(actual$tau, c(0.025, 0.5, 0.975))
    expect_identical(actual$model_id,
                     data.frame(team = c("ta", "ta", "tb"),
                                model = c("m1", "m2", "m1")))
})
