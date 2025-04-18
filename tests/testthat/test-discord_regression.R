get_p_value <- function(.results) {
  results_df <- summary(.results)
  results_df <- as.data.frame(results_df$coefficients)
  results_df <- cbind(names = rownames(results_df), results_df)
  rownames(results_df) <- NULL
  results_df[which(results_df$names == "y2_diff"), "Pr(>|t|)"]
}

signif_threshold <- 0.05

test_that("monozygotic significant is as expected", {
  set.seed(18)
  results <- discord_regression(mz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2")
  )

  expect_lt(object = get_p_value(results), expected = signif_threshold)
})

test_that("monozygotic nonsignificant is as expected", {
  set.seed(18)
  results <- discord_regression(mz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2")
  )

  expect_gt(object = get_p_value(results), expected = signif_threshold)
})

test_that("dizygotic significant is as expected", {
  set.seed(18)
  results <- discord_regression(dz_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2")
  )

  expect_lt(object = get_p_value(results), expected = signif_threshold)
})

test_that("dizygotic nonsignificant is as expected", {
  set.seed(18)
  results <- discord_regression(dz_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2")
  )

  expect_gt(object = get_p_value(results), expected = signif_threshold)
})


test_that("half siblings significant is as expected", {
  set.seed(18)
  results <- discord_regression(half_sibs_signif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2")
  )

  expect_lt(object = get_p_value(results), expected = signif_threshold)
})

test_that("half siblings nonsignificant is as expected", {
  set.seed(18)
  results <- discord_regression(half_sibs_nonsignif,
    outcome = "y1",
    predictors = "y2",
    id = "id",
    sex = NULL,
    race = NULL,
    pair_identifiers = c("_1", "_2")
  )

  expect_gt(object = get_p_value(results), expected = signif_threshold)
})
