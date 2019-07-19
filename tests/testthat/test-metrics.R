context("Performance metrics")

test_that("crossvalidation_metrics produces a data.frame", {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  model <- crossvalidate(data_matrix[keep,], labels[keep])

  met <- crossvalidation_metrics(model)
  expect_is(met, "data.frame")
})

test_that("Metric CIs contain Estimates, and are in [0, 1]", {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  model <- crossvalidate(data_matrix[keep,], labels[keep])

  metrics <- crossvalidation_metrics(model)

  for (i in 1:nrow(metrics)) {
    expect_lte(metrics[[i, "Lower 95%CI"]], metrics[[i, "Estimate"]])
    expect_gte(metrics[[i, "Upper 95%CI"]], metrics[[i, "Estimate"]])
    expect_true(all(metrics[i, -1] > 0))
    expect_true(all(metrics[i, -1] < 1))
  }
})

test_that("Metrics for a synthetic set of predictions are correct", {
  demi_classifier <- list(
    pred = data.frame(
      rowIndex = 1:8,
      pred = c("C0", "C0", "C0", "C1", "C1", "C1", "C1", "C0"),
      obs  = c("C0", "C0", "C0", "C0", "C1", "C1", "C1", "C1"),
      C0   = c(0.9,  0.8,  0.7,  0.4,  0.1,  0.2,  0.3,  0.6),
      C1   = c(0.1,  0.2,  0.3,  0.6,  0.9,  0.8,  0.7,  0.4),
      hp_1 = 1 # Dummy hyperparameter (code might break without at-least 1 hp)
    ),
    levels = c("C0", "C1"),
    trainingData = matrix(rnorm(8), ncol = 1) %>% magrittr::set_rownames(1:8)
  )

  metrics <- crossvalidation_metrics(demi_classifier)

  expect_equal(
    metrics[[which(metrics$Metric == "Sensitivity"), "Estimate"]],
    yardstick::sens(demi_classifier$pred, obs, pred)$.estimate
  )

  expect_equal(
    metrics[[which(metrics$Metric == "Specificity"), "Estimate"]],
    yardstick::spec(demi_classifier$pred, obs, pred)$.estimate
  )

  expect_equal(
    metrics[[which(metrics$Metric == "PPV"), "Estimate"]],
    yardstick::ppv(demi_classifier$pred, obs, pred)$.estimate
  )

  expect_equal(
    metrics[[which(metrics$Metric == "NPV"), "Estimate"]],
    yardstick::npv(demi_classifier$pred, obs, pred)$.estimate
  )

  expect_gt(
    metrics[[which(metrics$Metric == "AUC"), "Estimate"]],
    0.5
  )

})

test_that("Metrics equal ones re-calculated using confusion matrix entries", {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  model <- crossvalidate(data_matrix[keep,], labels[keep])

  metrics <- crossvalidation_metrics(model)

  mat <- yardstick::conf_mat(model$pred, obs, pred)$table
  A <- mat[1,1]
  B <- mat[1,2]
  C <- mat[2,1]
  D <- mat[2,2]

  Sensitivity = A/(A+C)
  Specificity = D/(B+D)
  Prevalence = (A+C)/(A+B+C+D)
  PPV = (Sensitivity * Prevalence) / ((Sensitivity * Prevalence) + ((1-Specificity) * (1-Prevalence)))
  NPV = (Specificity * (1-Prevalence)) / (((1-Sensitivity) * Prevalence) + ((Specificity) * (1-Prevalence)))

  expect_equal(
    metrics[[which(metrics$Metric == "Sensitivity"), "Estimate"]],
    Sensitivity
  )

  expect_equal(
    metrics[[which(metrics$Metric == "Specificity"), "Estimate"]],
    Specificity
  )

  expect_equal(
    metrics[[which(metrics$Metric == "PPV"), "Estimate"]],
    PPV
  )

  expect_equal(
    metrics[[which(metrics$Metric == "NPV"), "Estimate"]],
    NPV
  )
})