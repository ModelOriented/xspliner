get_formula_details <- function(formula) {

  response <- as.character(formula)[[2]]
  rhs_formula <- as.character(formula)[[3]]
  formula_variables <- all.vars(formula)
  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))
  additive_components <- as.character(attr(formula_terms,"variables"))[-c(1, 2)]

  list(
    formula = formula,
    raw_response = formula_variables[1],
    raw_pred_vars = formula_variables[-1],
    raponse = response,
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

  sprintf("%s_call[['%s']](%s)", fun, var, var)
}

get_component_params <- function(additive_component) {
  spline_params <- as.list(parse(text = additive_component[1])[[1]])
  spline_opts <- eval(spline_params$spline_opts)
  method_opts <- eval(spline_params$method_opts)
  list(
    spline_opts = spline_opts,
    method_opts = method_opts
  )
}

single_component_details <- function(raw_variable_name, additive_component) {

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
  component_params <- get_component_params(additive_component)

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
    single_component_details
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

  sprintf("%s ~ %s", formula_details$raponse, transformed_rhs)
}

single_component_env <- function(component_details) {
  blackbox_response_fun <- get_bb_response(component_details$method_opts)
  blackbox_response_approx <- get_bb_response_fun_approx(blackbox_response_fun, component_details$spline_opts)
  list(
    blackbox_response_fun,
    blackbox_response_approx
  )
}

common_components_env <- function() {

}

transformed_formula_object <- function(formula_details) {

  additive_components_details <- get_all_components_info(formula_details)
  transformed_formula_string <- transform_formula_chr(formula_details, additive_components_details)
  transformed_formula_env <-

  for (i in seq_along(formula_variables)) {
    details <- extract_call(formula_variables[i], vars[i])
    rhs_formula <- sub(details$call, details$new_call, rhs_formula, fixed = TRUE)
  }
  rhs_formula

  # xs_call <- list("x" = function(x) x ^ 2)
  # xf_call <- list("t" = function(t) sqrt(t))
  # enn <- new.env()
  # enn$xs_call <- xs_call
  # enn$xf_call <- xf_call
  form_final <- as.formula(sprintf("%s ~ %s", response, rhs_formula), env = env)
}
