#' Cross-validate on a matrix of ToF data with labels
#'
#' Perform 10-fold cross-validation on a matrix of ToF data. This produces
#' a 'caret::train' object containing lots of relevant information. One can
#' extract predictions made and use these to determine accuracy.
#'
#' The caret::train object also includes a classifier trained on the full
#' dataset. #' This can be used to make further predictions on new data, such as
#' a validation set.
#'
#' @param data_marix A matrix of ToF data
#' @param labels A character vector of class labels (shoulkd have 2 unique values)
#' @param model The model to use
#' @param n_folds number of folds in crossvalidation
#' @return A caret::train object
#' @export
crossvalidate <- function(data_matrix, labels, model = "xgbTree", n_folds = 10) {
  stopifnot(nrow(data_matrix) == length(labels))
  stopifnot(length(unique(labels)) == 2)

  train_control <- caret::trainControl(
    method          = "cv",
    number          = n_folds,
    verboseIter     = FALSE,
    summaryFunction = caret::twoClassSummary,
    classProbs      = TRUE,
    savePredictions = TRUE
  )

  hyperparameters <- list(
    "xgbTree" = data.frame(
        nrounds = 50,
        max_depth = 1,
        eta = 0.3,
        gamma = 0,
        colsample_bytree = 0.6,
        min_child_weight = 1,
        subsample = 0.75
      ),
    "ranger" = data.frame(
      mtry = 2,
      splitrule = "extratrees",
      min.node.size = 1.
    ),
    "glmnet" = data.frame(
        alpha = 0.325,
        lambda = 0.1
      ),
    "gaussprRadial" = data.frame(
      sigma = 0.2
    ),
    "nnet" = data.frame(
      size = 5,
      decay = 0.1
    ),
    "rf" = data.frame(
      mtry = 5
    ),
    "lda2" = data.frame(
      dimen = 1
    )
  )

  if(!(model %in% names(hyperparameters))) stop(paste0("'model' should be one of ", paste0(names(hyperparameters), collapse = ", ")))

  data_matrix <- drop_nearly_all_zero(as.matrix(data_matrix), 0.5)

  caret::train(
    x = data_matrix,
    y = labels,
    trControl = train_control,
    method    = model,
    metric    = "ROC",
    tuneGrid  = hyperparameters[[model]],
    preProcess = c("center", "scale")
  )
}

#' Crossvalidation ROC curve
#'
#' Produce a ROC curve based on the output from the 'crossvalidate' function.
#'
#' @param classifier Output from 'crossvalidate' function
#' @param title Title of the output plot
#' @return a ROC curve
#' @export
crossvalidation_roc <- function(classifier, title = "") {
  classifier_roc <- classifier$pred %>%
    yardstick::roc_curve(obs, classifier$levels[1])

  classifier_roc_ci <- pROC::ci(pROC::roc(
    response  = classifier$pred$obs,
    predictor = classifier$pred[,classifier$levels[1]],
    ci = TRUE
  ))

  ggplot2::autoplot(classifier_roc) +
    ggplot2::labs(title = title,
                  subtitle = paste0("AUC = ",
                                    round(classifier_roc_ci[2], 3),
                                    " (95%CI ",
                                    round(classifier_roc_ci[1], 3),
                                    "-",
                                    round(classifier_roc_ci[3], 3),
                                    ")")
    )
}

#' Crossvalidation metric
#'
#' Accuracy metrics for the cross-validated predictions made in 'crossvalidate'
#'
#' @param classifier Output of 'crossvalidate'
#' @return data frame of metrics
#' @export
crossvalidation_metrics <- function(classifier) {
  dplyr::bind_rows(
    yardstick::sens(classifier$pred, obs, pred),
    yardstick::spec(classifier$pred, obs, pred),
    yardstick::ppv(classifier$pred,  obs, pred),
    yardstick::npv(classifier$pred,  obs, pred)
  ) %>%
    dplyr::transmute(Metric = .metric, Estimate = round(.estimate, 3))
}

#' Crossvalidation feature importance
#'
#' Given a classifier output from 'crossvalidate', return feature importances
#' as either a plot or a data frame. Feature importances are calculated using
#' the caret::varImp function, which computes importances in a different way
#' depending on the classifier. I highly recommend reading the documentation
#' here: https://topepo.github.io/caret/variable-importance.html
#'
#' @param classifier Output from 'crossvalidate'
#' @param n_features The number of features to show
#' @param override_names A character vector to override the names of the
#'   features. This may be useful if the variable names are too long.
#' @param output_dataframe Default FALSE. If set to TRUE, this function returns
#'   a data frame of feature iportances instead of a plot.
#' @return Either a plot or a data frame of feature importance.
#' @export
crossvalidation_feature_importance <- function(classifier,
                                               n_features = 0,
                                               override_names = character(0),
                                               output_dataframe = FALSE) {
  importance <- caret::varImp(classifier)

  # If n_features is 0, set to all features
  n_features <- ifelse(n_features > 0, n_features, nrow(importance$importance))

  importance$importance <- head(importance$importance, n_features)

  if (length(override_names) > 0) {
    rownames(importance$importance)[1:length(override_names)] <- override_names
  }

  if (!output_dataframe) {
    return(ggplot(importance))
  } else {
    return(importance$importance)
  }
}
