#' Plot method for 'xspliner' model
#'
#' @description The method provides all plotting methods offered by 'xspliner' package.
#' See \link{plot_variable_transition} and \link{plot_model_comparison} for more details.
#'
#' @param x Object of class 'xspliner'
#' @param variable_names Names of predictors which transitions should be plotted.
#' @param model Base model that xspliner is based on.
#' @param plot_response If TRUE black box model response is drawn.
#' @param plot_approx If TRUE black box model response approximation is drawn.
#' @param data Training data used for building \code{x} model. Required for plot_data option and model comparing.
#' @param plot_data If TRUE raw data is drawn.
#' @param plot_deriv If TRUE derivative of approximation is showed on plot.
#' @param compare_with Named list. Other models that should be compared with xspliner and \code{model}.
#' @param n_plots Threshold for number of plots when plotting all variables.
#' @param sort_by When comparing models determines according to which model should observations be ordered.
#' @param prediction_funs Prediction functions that should be used in model comparison.
#' @param ... Another arguments passed into model specific method.
#'
#' @export
plot.xspliner <- function(x, variable_names = NULL, model = NULL, plot_response = TRUE, plot_approx = TRUE,
                    data = NULL, plot_data = FALSE, plot_deriv = FALSE, n_plots = 6, sort_by = NULL,
                    compare_with = list(), prediction_funs = list(function(object, newdata) predict(object, newdata)),
                    ...) {
  if (is.null(model)) {
    plot_variable_transition(x, variable_names, plot_response, plot_approx, data, plot_data, plot_deriv, n_plots)
  } else {
    if (is.null(data)) {
      stop("Data must be provided.")
    }
    plot_model_comparison(x, model, data, compare_with, prediction_funs, sort_by = sort_by)
  }
}

fit_r_sq <- function(surrogate_predicted, original_predicted) {
    1 - sum((surrogate_predicted - original_predicted) ^ 2) / sum((original_predicted - mean(original_predicted)) ^ 2)
}

fit_max_diff <- function(surrogate_predicted, original_predicted) {
  if (is.numeric(surrogate_predicted) && is.numeric(original_predicted)) {
    1 - max(abs(surrogate_predicted - original_predicted)) / diff(range(original_predicted))
  } else if (is.factor(surrogate_predicted) && is.factor(original_predicted)) {
    1 - mean(surrogate_predicted != original_predicted)
  } else {
    warning("Not consistent predictions provided.")
    NA
  }
}

measure_diff_roc <- function(a, b, measure = max) {
  diffs <- (a - b) ^ 2
  measure(sqrt(diffs[1, ] + diffs[2, ]))
}

fit_roc_diff <- function(surrogate_scores, original_scores, original_labels) {
  is_score_surrogate <- all(surrogate_scores <= 1) && all(surrogate_scores >=0)
  is_score_original <- all(original_scores <= 1) && all(original_scores >=0)
  if (is_score_original && is_score_surrogate) {
    roc_surrogate <- pROC::roc(original_labels, surrogate_scores, direction="<")
    roc_original <- pROC::roc(original_labels, original_scores, direction="<")
    thresholds <- union(roc_surrogate$thresholds, roc_original$thresholds)
    roc_surrogate_on_thresholds <- pROC::coords(
      roc_surrogate, x = thresholds, input = "threshold", ret = c("se", "1-sp"), transpose = TRUE
    )
    roc_original_on_thresholds <- pROC::coords(
      roc_original, x = thresholds, input = "threshold", ret = c("se", "1-sp"), transpose = TRUE
    )
    list(
      max = measure_diff_roc(roc_surrogate_on_thresholds, roc_original_on_thresholds),
      mean = measure_diff_roc(roc_surrogate_on_thresholds, roc_original_on_thresholds, measure = mean)
    )
  } else {
    message("Predictions are not scores. Skipping ROC comparison.")
    NULL
  }
}

compare_summary <- function(surrogate, original, surrogate_pred_fun, original_pred_fun, newdata, response) {
  surrogate_prediction <- surrogate_pred_fun(surrogate, newdata)
  original_prediction <- original_pred_fun(original, newdata)
  fit_max_simil <- fit_max_diff(surrogate_prediction, original_prediction)
  cat("Models comparison", "\n")
  if (attr(surrogate, "type") == "regression") {
    cat("  1 - Max prediction normed-diff: ", fit_max_simil, "\n")
    cat("  R^2: ", fit_r_sq(surrogate_prediction, original_prediction), "\n")
    cat("  MSE Black Box: ", mean((newdata[, response] - original_prediction) ^ 2), "\n")
    cat("  MSE Surrogate: ", mean((newdata[, response] - surrogate_prediction) ^ 2), "\n")
  } else {
    if (!is.numeric(original_prediction) && !is.numeric(surrogate_prediction)) {
      contingency_original <- table(newdata[, response], original_prediction)
      contingency_surrogate <- table(newdata[, response], surrogate_prediction)
      cat("  Mean predictions similarity: ", fit_max_simil, "\n")
      cat("  ACC Black Box: ", sum(diag(contingency_original)) / sum(contingency_original), "\n")
      cat("  ACC Surrogate: ", sum(diag(contingency_surrogate)) / sum(contingency_surrogate), "\n")
    }
    if (is.numeric(surrogate_prediction) && is.numeric(original_prediction)) {
      cat("  1 - Max prediction normed-diff: ", fit_max_simil, "\n")
      cat("  R^2: ", fit_r_sq(surrogate_prediction, original_prediction), "\n")
      measure_roc_diff <- fit_roc_diff(surrogate_prediction, original_prediction, newdata[, response])
      if (!is.null(measure_roc_diff)) {
        cat("  1 - Max ROC diff: ", 1 - measure_roc_diff$max, "\n")
        cat("  1 - Mean ROC diff: ", 1 - measure_roc_diff$mean, "\n")
      }
    }
  }
  return(invisible(NULL))
}

#' Summary method for xspliner object
#'
#' @param object xspliner object
#' @param predictor predictor for xspliner model formula
#' @param ... Another arguments passed into model specific method.
#' @param model Original black box model. Providing enables models comparison. See details.
#' @param newdata Data used for models comparison. By default training data used for black box build.
#' @param prediction_funs List of prediction functions for surrogate and black box model. For classification problem,
#'   different statistics are displayed based on predictions type. See details section for more info.
#' @param env Environment in which newdata is stored (if not provided as parameter).
#'
#' @details
#' The summary output depends strictly on data provided to it.
#'
#' Standard output for providing only xspliner model (object parameter) return default \code{glm::summary} output.
#'
#' Providing both xspliner model and predictor returns summary details for selecter variable.
#' The following points decribe the rules:
#' \itemize{
#'   \item{When variable was quantitative and transformed with fitted spline, the output contain approximation details.}
#'   \item{When variable was qualitative and transformed, factor matching is displayed.}
#'   \item{When variable was not transformed, glm::summary output is displayed for the model.}
#' }
#'
#' If both object parameter and model (original black box) was provided, the summary displays comparison of original and surrogate model.
#' The following points decribe the rules (\eqn{y_{s}}{y_s} and \eqn{y_{o}}{y_o} are predictions of surrogate and original model respectively on provided dataset).
#' When comparing statistic is close to 1, this means surrogate model is similiar to black box one (according to this statistic).
#'
#' For regression models:
#' \itemize{
#'   \item{1 - Maximum predictions normed-difference}{
#'     \deqn{1 - \frac{\max_{i = 1}^{n} |y_{s}^{(i)} - y_{o}^{(i)}|}{\max_{i = 1}^{n} y_{o}^{(i)} - \min_{i = 1}^{n} y_{o}^{(i)}}}{1 - (max(abs(y_s - y_o)) / diff(range(y_o)))}
#'   }
#'   \item{R^2 (\url{https://christophm.github.io/interpretable-ml-book/global.html#theory-4})}{
#'     \deqn{1 - \frac{\sum_{i = 1}^{n} ({y_{s}^{(i)} - y_{o}^{(i)}}) ^ {2}}{\sum_{i = 1}^{n} ({y_{o}^{(i)} - \overline{y_{o}}}) ^ {2}}}{1 - sum((y_s - y_o) ^ 2) / sum((y_o - mean(y_o)) ^ 2)}
#'   }
#'   \item{Mean square errors for each model.}
#' }
#'
#' For classification models the result depends on prediction type.
#' When predictions are classified levels:
#' \itemize{
#'   \item{Mean predictions similarity}{\deqn{\frac{1}{n} \sum_{i = 1}^{n} I_{y_{s}^{(i)} = y_{o}^{(i)}}}{mean(y_s == y_o)}}
#'   \item{Accuracies for each models.}
#' }
#'
#' When predictions are response probabilities:
#' \itemize{
#'   \item{R^2 as for regression model.}
#'   \item{1 - Maximum ROC difference}{\deqn{1 - \max_{t \in T} ||ROC_{o}(t) - ROC_{s}(t)||_{2}}{} Calculates maximum of euclidean distances between ROC points for specified thresholds set T. In this imlplementation T is union of breakpoints for each ROC curve.}
#'   \item{1 - Mean ROC difference}{ Above version using mean instead of max measure.}
#' }
#'
#' @examples
#' library(randomForest)
#' set.seed(1)
#' data <- iris
#' # regression model
#' iris.rf <- randomForest(Petal.Width ~  Sepal.Length + Petal.Length + Species, data = data)
#' iris.xs <- xspline(iris.rf)
#' # Summary of quantitative variable transition
#' summary(iris.xs, "Sepal.Length")
#' # Summary of qualitative variable transition
#' summary(iris.xs, "Species")
#' # Comparing surrogate with original model (regression)
#' summary(iris.xs, model = iris.rf, newdata = data)
#'
#' # Classification model
# data <- droplevels(iris[51:150, ]) # selecting only two species data
# iris.rf <- randomForest(Species ~ ., data = data)
# iris.xs <- xspline(iris.rf)
#
# # Comparing summaries requires providing prediction function
# # Prediction as probability for success
# prob_rf <- function(object, newdata) predict(object, newdata = newdata, type = "prob")[, 2]
# prob_xs <- function(object, newdata) predict(object, newdata = newdata, type = "response")
# summary(iris.xs, model = iris.rf, newdata = data, prediction_funs = list(prob_xs, prob_rf))
# # Prediction as final category
# response_rf <- function(object, newdata) predict(object, newdata = newdata)
# response_xs <- function(object, newdata) {
#   y_levels <- levels(newdata[[environment(object)$response]])
#   factor(
#     y_levels[(predict.glm(object, newdata = newdata, type = "link") > 0) + 1],
#     levels = y_levels
#   )
# }
# response_rf(iris.rf, newdata = data)
# response_xs(iris.xs, newdata = data)
# summary(iris.xs, model = iris.rf, newdata = data, prediction_funs = list(response_xs, response_rf))
#'
#' @export
summary.xspliner <- function(object, predictor, ..., model = NULL, newdata = NULL,
                             prediction_funs = list(function(object, newdata) predict(object, newdata)),
                             env = parent.frame()) {
  if (!is.null(model)) {
    data <- get_model_data(object, data, env)
    surrogate_pred_fun <- original_pred_fun <- prediction_funs[[1]]
    if (length(prediction_funs) > 1) {
      original_pred_fun <- prediction_funs[[2]]
    }
    return(compare_summary(object, model, surrogate_pred_fun, original_pred_fun, newdata, environment(object)$response))
  }
  if (missing(predictor)) {
    return(summary.glm(object, ...))
  }
  if (predictor %in% specials(object, "quantitative")) {
    return(mgcv::summary.gam(transition(object, predictor, "base"), ...))
  }
  if (predictor %in% specials(object, "qualitative")) {
    return(attributes(transition(object, predictor, "base"))$partition)
  }
  if (!(predictor %in% specials(object, "qualitative"))) {
    message("Variable was not transformed.")
    return(summary.glm(object, ...))
  }
}

#' Print method for xspliner object
#'
#' @param x xspliner object
#' @param predictor predictor for xspliner model formula
#' @param ... Another arguments passed into model specific print method.
#' @export
print.xspliner <- function(x, predictor, ...) {
  if (missing(predictor)) {
    class(x) <- "glm"
    return(print(x, ...))
  }
  if (predictor %in% specials(x, "quantitative")) {
    return(mgcv::print.gam(transition(x, predictor, "base"), ...))
  }
  if (predictor %in% specials(x, "qualitative")) {
    return(print(transition(x, predictor, "base"), ...))
  }
  if (!(predictor %in% specials(x, "qualitative"))) {
    message("Variable was not transformed.")
    class(x) <- "glm"
    return(print(x, ...))
  }
}

#' Extract variable transformation from xspliner
#'
#' @param model xspliner model
#' @param predictor variable name for which transformation should be extracted
#' @param type If 'function' then transformation function is extracted. For 'base' there is sourced
#'   object on which transformation was built - in case of quantitative variable GAM model, for qualitative
#'   factorMerger.
#' @param ... Other parameters passed to method. Currently not used.
#' @export
transition <- function(model, ...) {
  UseMethod("transition", model)
}

#' @rdname transition
#' @export
transition.xspliner <- function(model, predictor, type = "function", ...) {
  if (type == "function") {
    quantity_transition_function <- environment(model)$xs_functions[[predictor]]
    quality_transition_function <- environment(model)$xf_functions[[predictor]]
    if (!is.null(quantity_transition_function)) {
      return(quantity_transition_function)
    } else if (!is.null(quality_transition_function)) {
      return(quality_transition_function)
    } else {
      message("Variable is not transformed. Use identity.")
      return(function(x) x)
    }
  } else if (type == "base") {
    quantity_transition_object <- environment(model)$quantitative_transitions[[predictor]]$transition_outcome
    quality_transition_object <- environment(model)$qualitative_transitions[[predictor]]$transition_outcome
    if (!is.null(quantity_transition_object)) {
      return(quantity_transition_object)
    } else if (!is.null(quality_transition_object)) {
      return(quality_transition_object)
    } else {
      message("Variable is not transformed. NULL returned.")
      NULL
    }
  } else if (type == "data") {
    quantity_transition_object <- environment(model)$quantitative_transitions[[predictor]]$effect_outcome
    quality_transition_object <- environment(model)$qualitative_transitions[[predictor]]$effect_outcome
    if (!is.null(quantity_transition_object)) {
      return(quantity_transition_object)
    } else if (!is.null(quality_transition_object)) {
      return(quality_transition_object)
    } else {
      message("Variable is not transformed. NULL returned.")
      NULL
    }
  }
}
