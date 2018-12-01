#' @export
transition <- function(object, ...) {
  UseMethod("transition", object)
}

#' @export
transition.xspliner <- function(model, predictor, type = "function") {
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
      message("Variable is not transformed. Use identity.")
      NULL
    }
  }
}

specials <- function(xspliner, type = "all") {
  predictors_quant <- names(environment(model)$quantitative_transitions)
  predictors_qual <- names(environment(model)$qualitative_transitions)
  if (type == "quantitative") {
    return(predictors_quant)
  } else if (type == "qualitative") {
    return(predictors_qual)
  } else {
    c(predictors_quant, predictors_qual)
  }
}

