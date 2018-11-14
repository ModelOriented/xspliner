#' @export
approx_with_splines <- function(fun_data, pred_var, ...) {
  s <- mgcv::s
  names(fun_data)[1] <- "pred_var"
  #formula <- as.formula(sprintf("yhat ~ s(%s, ...)", pred_var), env = env)
  mgcv::gam(yhat ~ s(pred_var, ...), data = fun_data)
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
  spline_params[["fun_data"]] <- blackbox_response_obj # attr(blackbox_response_obj, "partial.data") do.call loses attributes
  spline_params[["pred_var"]] <- component_details$var
  #spline_params[["env"]] <- attr(formula_details$formula, ".Environment")

  blackbox_response_approx <- do.call(approx_with_splines, spline_params)
  list(
    blackbox_response_obj = blackbox_response_obj,
    blackbox_response_approx = blackbox_response_approx
  )
}

#' @export
single_component_env <- function(formula_details, component_details, blackbox, data) {
  if (component_details$method_opts$type == "pdp") {
    single_component_env_pdp(formula_details, component_details, blackbox, data)
  }
}

#' @export
common_components_env <- function(formula_details, special_components_details, blackbox, data) {

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

  if (length(xf_vars)) {
    xf_env <- special_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% xf_vars) %>%
      purrr::map(function(component_details) single_component_env(formula_details, component_details, blackbox, data)) %>%
      purrr::set_names(xf_vars)
  }

  list(
    xs_env = xs_env,
    xf_env = xf_env
  )
}

#' @export
get_xs_call <- function(xs_env, pred_var_name) {
  function(pred_var) {
    data <- data.frame(pred_var)
    names(data) <- "pred_var" # pred_var_name need to fix calling mgcv::s
    mgcv::predict.gam(xs_env$blackbox_response_approx, newdata = data)
  }
}

#' @export
get_xf_call <- function(xs_env, pred_var_name) {
  function(pred_var) {
    pred_var
  }
}
