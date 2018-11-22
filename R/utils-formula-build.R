#' @export
extract_formula_var_names <- function(formula, data) {
  formula_variables <- all.vars(formula)

  if (!missing(data)) {
    formula_variables <- formula_variables[formula_variables %in% names(data)]
  } else {
    warning("You didn't specify data within data.frame. Only vectors with same length will be treated as data source.")

    formula_env <- attr(formula, ".Environment")
    if (!(all(formula_variables %in% ls(formula_env)))) {
      stop("Not all data and parameters are defined in formula environment!")
    }

    is_data_vector <- function(variable) {
      env_var <- formula_env[[variable]]
      is.vector(env_var) && (length(env_var) > 1)
    }

    vector_vars <- formula_variables[purrr::map_lgl(formula_variables, is_data_vector)]
    vector_length <- purrr::map_int(vector_vars, ~ length(formula_env[[.]]))
    if (length(unique(vector_length)) != 1) {
      stop("Can not determine data variables. Not all vectors are the same length!")
    }

    formula_variables <- vector_vars
  }

  formula_variables
}

#' @export
get_formula_raw_components <- function(formula_terms) {
  as.character(attr(formula_terms,"variables"))[-c(1, 2)]
}

#' @export
get_formula_special <- function(variable_names, formula_terms, special, index = FALSE) {
  if (index) {
    attr(formula_terms, "specials")[[special]] - 1
  } else {
    variable_names[attr(formula_terms, "specials")[[special]]]
  }
}

#' @export
get_formula_details <- function(formula, variable_names) {

  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))

  list(
    formula = formula,
    raw_response_name = variable_names[1],
    raw_predictor_names = variable_names[-1],
    response = deparse(formula[[2]]),
    rhs_formula = gsub("\\s+", " ", trimws(paste0(deparse(formula[[3]]), collapse = ""))),
    additive_components = get_formula_raw_components(formula_terms),
    xs_variables = get_formula_special(variable_names, formula_terms, "xs"),
    xf_variables = get_formula_special(variable_names, formula_terms, "xf"),
    xs_variables_idx = get_formula_special(variable_names, formula_terms, "xs", TRUE),
    xf_variables_idx = get_formula_special(variable_names, formula_terms, "xf", TRUE)
  )
}

#' @export
prepare_call <- function(component_string, add_variable = TRUE) {
  fun <- substr(component_string, 1, 2)
  if (!(fun %in% c("xs", "xf"))) {
    return(component_string)
  }

  var <- sub("(,|\\)).*$", '', component_string) %>%
    substr(4, nchar(.))
  if (add_variable) {
    sprintf("%s(%s)", fun, var)
  } else {
    fun
  }

}

#' @export
get_component_params <- function(additive_component, env) {
  spline_params <- as.list(parse(text = additive_component[1])[[1]])
  transform_opts <- eval(spline_params$transform_opts, envir = env)
  method_opts <- eval(spline_params$method_opts, envir = env)
  list(
    transform_opts = transform_opts,
    method_opts = method_opts
  )
}

#' @export
get_special_component_details <- function(raw_variable_name, additive_component_chr, env) {

  transformed_component <- prepare_call(additive_component_chr)
  call_function <- prepare_call(additive_component_chr, FALSE)
  component_params <- get_component_params(additive_component_chr, env)

  component_details <- list(
    var = raw_variable_name,
    call = additive_component_chr,
    new_call = transformed_component,
    call_fun = call_function,
    transform_opts = component_params$transform_opts,
    method_opts = component_params$method_opts
  )

  component_details
}

#' @export
get_special_components_info <- function(formula_details) {
  special_predictor_indexes <- c(formula_details$xs_variables_idx, formula_details$xf_variables_idx)
  special_predictor_names <- formula_details$raw_predictor_names[special_predictor_indexes]
  special_predictor_additive_components <- formula_details$additive_components[special_predictor_indexes]

  special_component_details <- purrr::map2(
    special_predictor_names,
    special_predictor_additive_components,
    get_special_component_details,
    env = attr(formula_details$formula, ".Environment")
  )
  names(special_component_details) <- special_predictor_names
  special_component_details
}

#' @export
transform_formula_chr <- function(formula_details, special_components_details) {

  replace_component_call <- function(rhs_string_formula, component_details) {
    sub(component_details$call, component_details$new_call, rhs_string_formula, fixed = TRUE)
  }

  transformed_rhs <- purrr::reduce(
    special_components_details,
    replace_component_call,
    .init = formula_details$rhs_formula
  )

  sprintf("%s ~ %s", formula_details$response, transformed_rhs)
}

#' @export
transformed_formula_object <- function(formula_details, blackbox, data, alter, compare_stat) {

  special_components_details <- get_special_components_info(formula_details)
  transformed_formula_calls <- get_common_components_env(formula_details, special_components_details, blackbox, data)

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

  transformed_formula_env <- attr(formula_details$formula, ".Environment")
  transformed_formula_env$xs <- xs
  transformed_formula_env$xf <- xf
  transformed_formula_env$xs_call <- xs_call
  transformed_formula_env$xf_call <- xf_call
  transformed_formula_env$xs_env_list <- xs_env_list
  transformed_formula_env$xf_env_list <- xf_env_list
  transformed_formula_env$response_name <- formula_details$raw_response_name
  special_components_details <- correct_improved_components(
    alter, compare_stat, xs, xf, special_components_details, data, formula_details$response)
  transformed_formula_string <- transform_formula_chr(formula_details, special_components_details)

  as.formula(transformed_formula_string, env = transformed_formula_env)
}

#' @export
factor_opts_default = list(
  method_opts = list(type = "ice"),
  transform_opts = list(stat = "GIC", value = 3)
)

#' @export
numeric_opts_default = list(
  method_opts = list(type = "pdp"),
  transform_opts = list(k = 6)
)

build_predictor_component <- function(predictor, class, factor_opts, numeric_opts) {
  if (!(class %in% c("numeric", "integer", "factor"))) {
    stop("Wrong class passed.")
  }
  if (class == "factor") {
    sprintf(
      "xf(%s, method_opts = %s, transform_opts = %s)",
      predictor, deparse(factor_opts$method_opts), deparse(factor_opts$transform_opts)
    )
  } else {
    sprintf(
      "xs(%s, method_opts = %s, transform_opts = %s)",
      predictor, deparse(numeric_opts$method_opts), deparse(numeric_opts$transform_opts)
    )
  }
}

build_formula <- function(response, predictors, classes, factor_opts, numeric_opts) {
  rhs <- purrr::map2_chr(
    predictors, classes, build_predictor_component, factor_opts = factor_opts, numeric_opts = numeric_opts) %>%
    paste(collapse = " + ")

  sprintf("%s ~ %s", response, rhs)
}
