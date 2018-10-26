get_formula_details <- function(formula, data) {

  response <- as.character(formula)[[2]]
  rhs_formula <- as.character(formula)[[3]]
  formula_variables <- all.vars(formula)
  formula_variables <- formula_variables[formula_variables %in% names(data)]
  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))
  additive_components <- as.character(attr(formula_terms,"variables"))[-c(1, 2)]

  list(
    formula = formula,
    raw_response = formula_variables[1],
    raw_pred_vars = formula_variables[-1],
    response = response,
    rhs_formula = rhs_formula,
    additive_components = additive_components,
    xs_variables = formula_variables[attr(formula_terms, "specials")$xs],
    xf_variables = formula_variables[attr(formula_terms, "specials")$xf]
  )
}

prepare_call <- function(string) {
  fun <- substr(string, 1, 2)
  if (!(fun %in% c("xs", "xf"))) {
    return(string)
  }

  var <- sub("(,|\\)).*$", '', string) %>%
    substr(4, nchar(.))

  sprintf("%s(%s)", fun, var)
}

get_component_params <- function(additive_component, env) {
  spline_params <- as.list(parse(text = additive_component[1])[[1]])
  spline_opts <- eval(spline_params$spline_opts, envir = env)
  method_opts <- eval(spline_params$method_opts, envir = env)
  list(
    spline_opts = spline_opts,
    method_opts = method_opts
  )
}

single_component_details <- function(raw_variable_name, additive_component, env) {

  if (raw_variable_name == additive_component) {
    return(
      list(
        var = raw_variable_name,
        call = additive_component,
        new_call = raw_variable_name,
        spline_opts = NULL,
        method_opts = NULL
      )
    )
  }

  transformed_component <- prepare_call(additive_component)
  component_params <- get_component_params(additive_component, env)

  component_details <- list(
    var = raw_variable_name,
    call = additive_component,
    new_call = transformed_component,
    spline_opts = component_params$spline_opts,
    method_opts = component_params$method_opts
  )
  attr(component_details, "xp_detail") <- TRUE

  component_details
}

get_all_components_info <- function(formula_details) {
  additive_component_details <- purrr::map2(
    formula_details$raw_pred_vars,
    formula_details$additive_components,
    single_component_details,
    env = attr(formula_details$formula, ".Environment")
  )
  names(additive_component_details) <- formula_details$raw_pred_vars
  additive_component_details
}

transform_formula_chr <- function(formula_details, additive_components_details) {

  replace_component_call <- function(rhs_string_formula, component_details) {

    if (is.null(attr(component_details, "xp_detail"))) {
      return(rhs_string_formula)
    }

    sub(component_details$call, component_details$new_call, rhs_string_formula, fixed = TRUE)
  }

  transformed_rhs <- purrr::reduce(
    additive_components_details,
    replace_component_call,
    .init = formula_details$rhs_formula
  )

  sprintf("%s ~ %s", formula_details$response, transformed_rhs)
}

approx_with_splines <- function(fun_data, pred_var, ...) {
  s <- mgcv::s
  names(fun_data)[1] <- "pred_var"
  #formula <- as.formula(sprintf("yhat ~ s(%s, ...)", pred_var), env = env)
  mgcv::gam(yhat ~ s(pred_var, ...), data = fun_data)
}

single_component_env_pdp <- function(formula_details, component_details, blackbox, data) {
  methos_params <- component_details$method_opts
  methos_params[["type"]] <- NULL
  methos_params[["object"]] <- blackbox
  methos_params[["pred.var"]] <- component_details$var
  methos_params[["train"]] <- data

  blackbox_response_obj <- do.call(pdp::partial, methos_params)

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

single_component_env <- function(formula_details, component_details, blackbox, data) {
  if (component_details$method_opts$type == "pdp") {
    single_component_env_pdp(formula_details, component_details, blackbox, data)
  }
}

common_components_env <- function(formula_details, additive_components_details, blackbox, data) {

  xs_env <- list()
  xf_env <- list()

  xs_vars <- formula_details$xs_variables
  xf_vars <- formula_details$xf_variables

  if (length(xs_vars)) {
    xs_env <- additive_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% xs_vars) %>%
      purrr::map(function(component_details) single_component_env(formula_details, component_details, blackbox, data)) %>%
      purrr::set_names(xs_vars)
  }

  if (length(xf_vars)) {
    xf_env <- additive_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% xf_vars) %>%
      purrr::map(function(component_details) single_component_env(formula_details, component_details, blackbox, data)) %>%
      purrr::set_names(xf_vars)
  }

  list(
    xs_env = xs_env,
    xf_env = xf_env
  )
}

get_xs_call <- function(xs_env, pred_var_name) {
  function(pred_var) {
    data <- data.frame(pred_var)
    names(data) <- "pred_var" # pred_var_name need to fix calling mgcv::s
    mgcv::predict.gam(xs_env$blackbox_response_approx, newdata = data)
  }
}

get_xf_call <- function(xs_env, pred_var_name) {
  function(pred_var) {
    pred_var
  }
}

transformed_formula_object <- function(formula_details, blackbox, data) {

  additive_components_details <- get_all_components_info(formula_details)
  transformed_formula_string <- transform_formula_chr(formula_details, additive_components_details)
  transformed_formula_calls <- common_components_env(formula_details, additive_components_details, blackbox, data)

  transformed_formula_env <- attr(formula_details$formula, ".Environment")
  xs_env_list <- transformed_formula_calls$xs_env
  xs_call <- purrr::map2(xs_env_list, names(xs_env_list), get_xs_call) %>%
    purrr::set_names(names(xs_env_list))
  xs <- function(variable) {
    var_name <- deparse(substitute(variable))
    xs_call[[var_name]](variable)
  }

  xf_env_list <- transformed_formula_calls$xf_env
  xf_call <- purrr::map2(xf_env_list, names(xf_env_list), get_xf_call) %>%
    purrr::set_names(names(xf_env_list))
  xf <- function(variable) {
    var_name <- deparse(substitute(variable))
    xf_call[[var_name]](variable)
  }

  transformed_formula_env$xs <- xs
  transformed_formula_env$xf <- xf
  list(
    transformed_formula = as.formula(
      transformed_formula_string,
      env = transformed_formula_env),
    xs_env = xs_env_list,
    xf_env = xf_env_list
  )

}

xp_gam <- function(formula, blackbox, data = model.frame(blackbox), env = parent.frame()) {
  attr(formula, ".Environment") <- env
  formula_details <- get_formula_details(formula, data)
  transformed_formula <- transformed_formula_object(formula_details, blackbox, data)
  mgcv::gam(transformed_formula$transformed_formula, data = data)
}

xp_gam_predict <- function(xp_gam_model, newdata) {
  mgcv::predict.gam(xp_gam_model, newdata = newdata)
}
