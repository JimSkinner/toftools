get_hyperparameters <- function(model) {
  labels      <- pigs[[1]] %>% stringr::str_sub(1, 2)
  keep        <- labels %in% c("F0", "F3")

  labels      <- labels[keep]
  data_matrix <- drop_nearly_all_zero(as.matrix(pigs[keep, -1]), 0.1)

  train_control <- caret::trainControl(
    method          = "repeatedcv",
    number          = 10,
    repeats         = 5,
    verboseIter     = FALSE,
    summaryFunction = caret::twoClassSummary,
    classProbs      = TRUE,
    savePredictions = TRUE
  )

  caret::train(
    x = data_matrix,
    y = labels,
    trControl = train_control,
    method    = model,
    metric    = "ROC",
    tuneLength = 5,
    preProcess = c("center", "scale", "BoxCox")
  )
}
