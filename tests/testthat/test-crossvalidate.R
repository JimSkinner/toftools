context("crossvalidate")

test_that("crossvalidate produces a trained classifier", {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  models <- c(
    "xgbTree",
    "rf",
    "ranger",
    "glmnet",
    "gaussprRadial",
    "nnet",
    "lda2"
  )

  for (model in models) {
    classifier <- crossvalidate(data_matrix[keep,], labels[keep], model = model)
    expect_is(classifier, "train")
  }
})

test_that("crossvalidation_roc produces a ggplot object", {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  model <- crossvalidate(data_matrix[keep,], labels[keep])

  plt <- crossvalidation_roc(model)
  expect_is(plt, "ggplot")
})

test_that("crossvalidation_metrics produces a data.frame", {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  model <- crossvalidate(data_matrix[keep,], labels[keep])

  met <- crossvalidation_metrics(model)
  expect_is(met, "data.frame")
})

test_that("crossvalidation_feature_importance gives expected output for different parameter values", {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  classifier <- crossvalidate(data_matrix[keep,], labels[keep])

  crossvalidation_feature_importance(classifier) %>%
    expect_is("ggplot")

  crossvalidation_feature_importance(classifier, output_dataframe = TRUE) %>%
    expect_is("data.frame")

  crossvalidation_feature_importance(classifier,
                                     n_features = 10,
                                     output_dataframe = TRUE) %>%
    dim() %>%
    expect_equal(c(10, 1))

  crossvalidation_feature_importance(classifier,
                                     n_features = 5,
                                     override_names = LETTERS[1:5],
                                     output_dataframe = TRUE) %>%
    rownames() %>%
    expect_equal(LETTERS[1:5])
})

test_that("crossvalidate gives a very high AUC when a clear signal is injected" , {
  data_matrix <- pigs[,-1]
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  data_matrix <- data_matrix[keep,]
  labels      <- labels[keep]

  data_matrix[labels == "F3",] <- data_matrix[labels == "F3",] + 10000

  classifier <- crossvalidate(data_matrix, labels)

  expect_gt(classifier$results$ROC, 0.95)
})
