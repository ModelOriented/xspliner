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
    response = as.character(formula)[[2]],
    rhs_formula = as.character(formula)[[3]],
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
  spline_opts <- eval(spline_params$spline_opts, envir = env)
  method_opts <- eval(spline_params$method_opts, envir = env)
  list(
    spline_opts = spline_opts,
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
    spline_opts = component_params$spline_opts,
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
is_lm_better_than_approx <- function(data, response, predictor, approx_fun, compare_stat) {
  approx_model_formula <- as.formula(sprintf("%s ~ approx_fun(%s)", response, predictor))
  approx_model <- lm(approx_model_formula, data)
  lm_model_formula <- as.formula(sprintf("%s ~ %s", response, predictor))
  lm_model <- lm(lm_model_formula, data)
  compare_stat(approx_model) <= compare_stat(lm_model)
}

#' @export
choose_improved_components <- function(auto_approx, compare_stat, xs, xf, special_components_details, data, response) {
  if (!auto_approx) {
    return(special_components_details)
  }
  get_component_call <- function(special_component_details) {
    if (special_component_details$call_fun == "xs") {
      xs
    } else {
      xf
    }
  }

  use_untransformed <- function(special_component_details) {
    is_lm_better_than_approx(
      data,
      response,
      special_component_details$var,
      get_component_call(special_component_details),
      compare_stat
    )
  }

  use_bare_call <- function(special_component_details) {
    special_component_details$new_call <- special_component_details$var
    special_component_details
  }

  special_components_details %>%
    purrr::map_if(use_untransformed, use_bare_call)

}

#' @export
transformed_formula_object <- function(formula_details, blackbox, data, auto_approx, compare_stat) {

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
  special_components_details <- choose_improved_components(
    auto_approx, compare_stat, xs, xf, special_components_details, data, formula_details$response)
  transformed_formula_string <- transform_formula_chr(formula_details, special_components_details)

  as.formula(transformed_formula_string, env = transformed_formula_env)
}
