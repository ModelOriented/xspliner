#' Plot method for 'xspliner' model
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

  if (is.null(variable_names) && is.null(model)) {
    special_vars <- specials(x, "all")
    special_vars_to_plot <- special_vars[1:min(n_plots, length(special_vars))]
    plot_specials_grid(x, special_vars_to_plot, plot_response, plot_approx, data, plot_data, plot_deriv)
  } else if (length(variable_names) > 1 && is.null(model)) {
    special_vars <- specials(x, "all")
    special_vars_to_plot <- intersect(special_vars, variable_names)
    if (length(special_vars_to_plot) == 0) {
      stop("None of selected variables was transformed.")
    }
    plot_specials_grid(x, special_vars_to_plot, plot_response, plot_approx, data, plot_data, plot_deriv)
  } else if (is.null(variable_names) && !is.null(model)) {
    if (is.null(data)) {
      stop("Data must be provided.")
    }
    plot_model_comparison(x, model, data, compare_with, prediction_funs, sort_by = sort_by)
  } else if (!(variable_names %in% specials(x, "all"))) {
    stop("Variable wasn't transformed.")
  } else if (variable_names %in% specials(x, "qualitative")) {
    plot(transition(x, variable_names, "base"))
  } else {
    plot_quantitative(x, variable_names, plot_response, plot_approx, data, plot_data, plot_deriv)
  }
}

#' Summary method for xspliner object
#'
#' @param object xspliner object
#' @param predictor predictor for xspliner model formula
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
