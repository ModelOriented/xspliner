
#' @export
get_spline_formula <- function(response_var, pred_var, env, ...) {
  formula_call <- substitute(list(pred_var, ...))
  formula_call[[1]] <- quote(s)
  formula_call[[2]] <- quote(predictor)
  formula_call <- sub("predictor", pred_var, deparse(formula_call), fixed = TRUE)
  formula <- as.formula(sprintf("%s ~ %s", response_var, formula_call), env = env)
  formula
}

#' @export
approx_with_splines <- function(bb_response_data, response_var, pred_var, env, ...) {
  s <- mgcv::s
  formula <- get_spline_formula(response_var, pred_var, env, ...)
  mgcv::gam(formula, data = bb_response_data)
}

#' @export
single_component_env_pdp <- function(formula_details, component_details, blackbox, data) {
  method_params <- component_details$method_opts
  method_params[["type"]] <- NULL
  method_params[["object"]] <- blackbox
  method_params[["pred.var"]] <- component_details$var
  method_params[["train"]] <- data

  blackbox_response_obj <- do.call(pdp::partial, method_params)

  spline_params <- component_details$spline_opts
  spline_params[["bb_response_data"]] <- blackbox_response_obj # attr(blackbox_response_obj, "partial.data") do.call loses attributes
  spline_params[["pred_var"]] <- component_details$var
  spline_params[["response_var"]] <- "yhat"
  spline_params[["env"]] <- attr(formula_details$formula, ".Environment")

  blackbox_response_approx <- do.call(approx_with_splines, spline_params)
  list(
    blackbox_response_obj = blackbox_response_obj,
    blackbox_response_approx = blackbox_response_approx
  )
}

#' @export
single_component_env_ale <- function(formula_details, component_details, blackbox, data) {
  method_params <- component_details$method_opts
  method_params[["type"]] <- NULL
  method_params[["X.model"]] <- blackbox
  method_params[["J"]] <- component_details$var
  method_params[["X"]] <- data
  method_params[["pred.fun"]] <- function(X.model, newdata) predict(object = X.model, newdata = newdata)
  method_params[["NA.plot"]] <- FALSE

  blackbox_response_obj <- do.call(ALEPlot::ALEPlot, method_params)
  blackbox_response_obj <- data.frame(blackbox_response_obj$x.values, blackbox_response_obj$f.values)
  names(blackbox_response_obj) <- c(component_details$var, "yhat")

  spline_params <- component_details$spline_opts
  spline_params[["bb_response_data"]] <- blackbox_response_obj
  spline_params[["pred_var"]] <- component_details$var
  spline_params[["response_var"]] <- "yhat"
  spline_params[["env"]] <- attr(formula_details$formula, ".Environment")

  blackbox_response_approx <- do.call(approx_with_splines, spline_params)
  list(
    blackbox_response_obj = blackbox_response_obj,
    blackbox_response_approx = blackbox_response_approx
  )
}

#' @export
single_component_env <- function(formula_details, component_details, blackbox, data) {
  if (is.null(component_details$method_opts$type)) {
    stop("No specified type for method!")
  }

  switch(component_details$method_opts$type,
    pdp = single_component_env_pdp(formula_details, component_details, blackbox, data),
    ale = single_component_env_ale(formula_details, component_details, blackbox, data)
  )

}

#' @export
get_common_components_env <- function(formula_details, special_components_details, blackbox, data) {

  xs_env <- list()
  xf_env <- list()

  xs_vars <- formula_details$xs_variables
  xf_vars <- formula_details$xf_variables

  if (length(xs_vars)) {
    xs_env <- special_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% xs_vars) %>%
      purrr::map(function(component_details) single_component_env(formula_details, component_details, blackbox, data)) %>%
      purrr::set_names(xs_vars)
  }

  # (todo)
  # if (length(xf_vars)) {
  #   xf_env <- special_components_details %>%
  #     purrr::keep(function(component_details) component_details[["var"]] %in% xf_vars) %>%
  #     purrr::map(function(component_details) single_component_env(formula_details, component_details, blackbox, data)) %>%
  #     purrr::set_names(xf_vars)
  # }

  list(
    xs_env = xs_env,
    xf_env = xf_env
  )
}

#' @export
get_xs_call <- function(xs_env, pred_var_name) {
  function(pred_var) {
    data <- data.frame(pred_var)
    names(data) <- pred_var_name
    mgcv::predict.gam(xs_env$blackbox_response_approx, newdata = data)
  }
}

#' @export
get_xf_call <- function(xs_env, pred_var_name) {
  function(pred_var) {
    pred_var
  }
}
