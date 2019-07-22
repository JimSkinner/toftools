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
#' @param tune Whether to tune hyper-parameters (TRUE), or use defaults (FALSE)
#' @param hyperparams One of 'tof' or 'gcims' whether the default hyper-
#'   parameters have been picked for tof data or gc-ims data. No effect if tune
#'   = TRUE.
#' @return A caret::train object
#' @export
crossvalidate <- function(data_matrix, labels, model = "xgbTree", n_folds = 10,
                          tune = FALSE, hyperparams = "tof") {
  stopifnot(nrow(data_matrix) == length(labels))
  stopifnot(length(unique(labels)) == 2)

  data_matrix <- drop_nearly_all_zero(as.matrix(data_matrix), 0.5)

  train_control <- caret::trainControl(
    method          = "cv",
    number          = n_folds,
    verboseIter     = FALSE,
    summaryFunction = caret::twoClassSummary,
    classProbs      = TRUE,
    savePredictions = TRUE
  )

  if (model == "glmnet") {
    preProcess <- c("center", "scale")
  } else {
    preProcess <- c("center", "scale", "BoxCox")
  }

  if (!tune) {
    if (hyperparams == "tof") {
      hyperparameters <- list(
        "xgbTree"       = data.frame(nrounds = 50,
                                     max_depth = 1,
                                     eta = 0.3,
                                     gamma = 0,
                                     colsample_bytree = 0.6,
                                     min_child_weight = 1,
                                     subsample = 0.75),
        "ranger"        = data.frame(mtry = 2,
                                     splitrule = "extratrees",
                                     min.node.size = 1),
        "glmnet"        = data.frame(alpha = 0.325,
                                     lambda = 0.1),
        "gaussprRadial" = data.frame(sigma = 0.2),
        "nnet"          = data.frame(size = 5,
                                     decay = 0.1),
        "rf"            = data.frame(mtry = 5),
        "lda2"          = data.frame(dimen = 1),
        "svmRadial"     = data.frame(sigma  = 0.1454651,
                                     C = 0.25)
      )
    } else if (hyperparams == "gcims") {
      hyperparameters <- list(
        "xgbTree"       = data.frame(nrounds = 50,
                                     max_depth = 2,
                                     eta = 0.4,
                                     gamma = 0,
                                     colsample_bytree = 0.6,
                                     min_child_weight = 1,
                                     subsample = 1),
        "glmnet"        = data.frame(alpha = 1,
                                     lambda = 0.1475432)
      )
    } else {
      stop("hyperparams must be one of 'tof'/'gcims'")
    }

    if(!(model %in% names(hyperparameters))) stop(paste0("'model' should be one of ", paste0(names(hyperparameters), collapse = ", ")))

    classifier <- caret::train(
      x = data_matrix,
      y = labels,
      trControl = train_control,
      method    = model,
      metric    = "ROC",
      tuneGrid  = hyperparameters[[model]],
      preProcess = preProcess
    )
  } else {
    classifier <- caret::train(
      x = data_matrix,
      y = labels,
      trControl = train_control,
      method    = model,
      metric    = "ROC",
      tuneLength = 4,
      preProcess = preProcess
    )
  }
  return(classifier)
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
  classifier_roc <- classifier %>%
    crossvalidation_predictive_probabilities() %>%
    yardstick::roc_curve(truth = obs,
                         classifier$levels[1],
                         options = list(
                           direction = "<",
                           transpose = TRUE
                         ))

  auc <- crossvalidation_metrics(classifier) %>%
    dplyr::filter(Metric == "AUC")

  ggplot2::autoplot(classifier_roc) +
    ggplot2::labs(title = title,
                  subtitle = paste0("AUC = ",
                                    auc %>% dplyr::pull("Estimate") %>% round(3),
                                    " (95%CI ",
                                    auc %>% dplyr::pull("Lower 95%CI") %>% round(3),
                                    "-",
                                    auc %>% dplyr::pull("Upper 95%CI") %>% round(3),
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
  # stopifnot(nrow(classifier$results) == 1) # Does not work for multiple HP vals

  pos_class <- classifier$levels[1]

  safe_auc <- purrr::possibly(yardstick::roc_auc, list(.estimate = NA_real_))

  get_metrics <- function(pred) {
    pred %>%
      dplyr::summarise(
        sens = yardstick::sens_vec(obs, pred),
        spec = yardstick::spec_vec(obs, pred),
        ppv  = yardstick::ppv_vec(obs, pred),
        npv  = yardstick::npv_vec(obs, pred),
        auc  = safe_auc(., obs, !!pos_class, options = list(transpose = TRUE, direction = "<"))$.estimate
      )
  }

  get_boot_metrics <- function(split) {
    rsample::analysis(split) %>%
      get_metrics()
  }

  boot_tbl <- classifier %>%
    crossvalidation_predictive_probabilities() %>%
    rsample::bootstraps(times = 2000) %>%
    dplyr::mutate(metrics = purrr::map(splits, get_boot_metrics)) %>%
    tidyr::unnest(metrics)

  ci95_tbl <- boot_tbl %>%
    dplyr::summarise(
      sens_lower = quantile(sens, probs = c(0.05), na.rm = TRUE),
      spec_lower = quantile(spec, probs = c(0.05), na.rm = TRUE),
      ppv_lower  = quantile(ppv,  probs = c(0.05), na.rm = TRUE),
      npv_lower  = quantile(npv,  probs = c(0.05), na.rm = TRUE),
      auc_lower  = quantile(auc,  probs = c(0.05), na.rm = TRUE),
      sens_upper = quantile(sens, probs = c(0.95), na.rm = TRUE),
      spec_upper = quantile(spec, probs = c(0.95), na.rm = TRUE),
      ppv_upper  = quantile(ppv,  probs = c(0.95), na.rm = TRUE),
      npv_upper  = quantile(npv,  probs = c(0.95), na.rm = TRUE),
      auc_upper  = quantile(auc,  probs = c(0.95), na.rm = TRUE)
    )

  tibble::tibble(
    Metric     = c("Sensitivity", "Specificity", "PPV", "NPV", "AUC"),
    Estimate   = unlist(get_metrics(classifier$pred)),
    "Lower 95%CI" = c(
      ci95_tbl$sens_lower,
      ci95_tbl$spec_lower,
      ci95_tbl$ppv_lower,
      ci95_tbl$npv_lower,
      ci95_tbl$auc_lower
    ),
    "Upper 95%CI" = c(
      ci95_tbl$sens_upper,
      ci95_tbl$spec_upper,
      ci95_tbl$ppv_upper,
      ci95_tbl$npv_upper,
      ci95_tbl$auc_upper
    )
  )
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

#' Crossvalidation predictive probabilities
#'
#' Given a classifier output by 'crossvalidate', return a tibble with one row
#' for each training sample containing the sample name, observed class,
#' predicted class, as well as the predicted probability of each class.
#' @param classifier output from 'crossvalidate' function
#' @export
crossvalidation_predictive_probabilities <- function(classifier) {
  params <- classifier$bestTune
  best_performing_fold <- rep(TRUE, nrow(classifier$pred))
  for (param in names(params)) {
    non_best <- classifier$pred[,param] != params[[param]]
    best_performing_fold[non_best] <- FALSE
  }
  classifier$pred[best_performing_fold,] %>%
    dplyr::arrange(rowIndex) %>%
    dplyr::mutate(sample = rownames(classifier$trainingData)) %>%
    dplyr::select(sample, obs, pred, !!!classifier$levels)
}