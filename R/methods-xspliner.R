#' Plot method for 'xspliner' model
#'
#' @param x Object of class 'xspliner'
#' @param variable_name Name of predictor that should be plotted.
#' @param model Base model that xspliner is based on.
#' @param plot_response If TRUE blackbox model response is drawn.
#' @param plot_approx If TRUE blackbox model response approcimation is drawn.
#' @param plot_data If TRUE raw data is drawn.
#' @param data Training data used for building \code{x} model. Required for plot_data option and model comparing.
#' @param compare_with Named list. Other models that should be compared with xspliner and \code{model}.
#' @param prediction_funs Prediction functions that should be used in model comparison.
#' @param ... Another arguments passed into model specific method.
#'
#' @export
plot.xspliner <- function(x, variable_name = NULL, model = NULL, plot_response = TRUE, plot_approx = TRUE,
                    data = NULL, plot_data = FALSE, plot_deriv = TRUE, compare_with = list(),
                    prediction_funs = list(function(object, newdata) predict(object, newdata)),
                    ...) {

  if (is.null(variable_name) && is.null(model)) {
    class(x) <- "lm"
    plot(x, ...)
  } else if (is.null(variable_name) && !is.null(model)) {
    if (is.null(data)) {
      stop("Data must be provided.")
    }
    plot_model_comparison(x, model, data, compare_with, prediction_funs)
  } else if (!(variable_name %in% specials(x, "all"))) {
    stop("Variable wasn't transformed.")
  } else if (variable_name %in% specials(x, "qualitative")) {
    plot(transition(x, variable_name, "base"))
  } else {
    plot_quantitative(x, variable_name, plot_response, plot_approx, data, plot_data, plot_deriv)
  }
}

#' Summary method for xspliner object
#'
#' @param object xspliner object
#' @param predictor precitor for xspliner model formula
#' @param ... Another arguments passed into model specific method.
#' @export
summary.xspliner <- function(object, predictor, ...) {
  if (missing(predictor)) {
    return(summary.glm(object, ...))
  }
  if (predictor %in% specials(object, "quantitative")) {
    return(mgcv::summary.gam(transition(object, predictor, "base"), ...))
  }
  if (predictor %in% specials(object, "qualitative")) {
    return(attributes(transition(object, predictor, "base"))$partition) # (todo) write own method
  }
  if (!(predictor %in% specials(object, "qualitative"))) {
    message("Variable was not transformed.")
    return(summary.glm(object, ...))
  }
}

#' Print method for xspliner object
#'
#' @param x xspliner object
#' @param predictor precitor for xsplner model formula
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
