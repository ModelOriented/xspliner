#' @export
extract_xspliner_function <- function(object, ...) {
  UseMethod("extract_xspliner_function", object)
}

#' @export
extract_xspliner_function.xspliner <- function(model, predictor) {
  quantity_transition_function <- environment(model)$xs_functions[[predictor]]
  quality_transition_function <- environment(model)$xf_functions[[predictor]]
  if (!is.null(quantity_transition_function)) {
    return(quantity_transition_function)
  } else if (!is.null(quantity_transition_function)) {
    return(quantity_transition_function)
  } else {
    message("Variable is not transformed. Use identity.")
    return(function(x) x)
  }
}

