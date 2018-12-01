#' To avoid CRAN check problems
utils::globalVariables(".")

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

get_formula_single_components <- function(formula_terms) {
  as.character(attr(formula_terms,"variables"))[-c(1, 2)]
}

get_special_predictors <- function(variable_names, formula_terms, special, index = FALSE) {
  if (index) {
    attr(formula_terms, "specials")[[special]] - 1
  } else {
    variable_names[attr(formula_terms, "specials")[[special]]]
  }
}

get_formula_metadata <- function(formula, variable_names) {

  if (!(length(formula) == 3)) {
    stop("Not specified response in formula")
  }

  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))

  formula_metadata <- list(
    formula = formula,
    response = variable_names[1],
    predictors = variable_names[-1],
    lhs = get_formula_lhs(formula),
    rhs = get_formula_rhs(formula),
    additive_components = get_formula_single_components(formula_terms),
    xs_variables = get_special_predictors(variable_names, formula_terms, "xs"),
    xf_variables = get_special_predictors(variable_names, formula_terms, "xf"),
    xs_variables_idx = get_special_predictors(variable_names, formula_terms, "xs", TRUE),
    xf_variables_idx = get_special_predictors(variable_names, formula_terms, "xf", TRUE)
  )
  if (length(formula_metadata$predictors) != length(formula_metadata$additive_components)) {
    stop("Number of predictors is different than additive components")
  }

  formula_metadata
}

clear_special_component <- function(component_string, add_variable = TRUE) {
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

get_component_params <- function(additive_component, xs_opts, xf_opts, env) {
  spline_params <- as.list(parse(text = additive_component[1])[[1]])
  transition <- eval(spline_params$transition, envir = env)
  effect <- eval(spline_params$effect, envir = env)
  transition_type <- substr(additive_component, 1, 2)
  if (transition_type == "xs") {
    transition <- match_parameters(transition, xs_opts$transition, xs_opts_default$transition)
    effect <- match_parameters(effect, xs_opts$effect, xs_opts_default$effect)
  } else {
    transition <- match_parameters(transition, xf_opts$transition, xf_opts_default$transition)
    effect <- match_parameters(effect, xf_opts$effect, xf_opts_default$effect)
  }

  list(
    transition = transition,
    effect = effect
  )
}

get_special_component_metadata <- function(raw_variable_name, additive_component_chr, xs_opts, xf_opts, env) {

  transformed_component <- clear_special_component(additive_component_chr)
  call_function <- clear_special_component(additive_component_chr, FALSE)
  component_params <- get_component_params(additive_component_chr, xs_opts, xf_opts, env)

  component_details <- list(
    var = raw_variable_name,
    call = additive_component_chr,
    new_call = transformed_component,
    call_fun = call_function,
    transition = component_params$transition,
    effect = component_params$effect
  )

  component_details
}

collect_specials_metadata <- function(formula_metadata, xs_opts, xf_opts) {
  special_predictor_indexes <- c(formula_metadata$xs_variables_idx, formula_metadata$xf_variables_idx)
  special_predictor_names <- formula_metadata$predictors[special_predictor_indexes]
  special_predictor_additive_components <- formula_metadata$additive_components[special_predictor_indexes]

  special_component_metadata <- purrr::map2(
    special_predictor_names,
    special_predictor_additive_components,
    get_special_component_metadata,
    xs_opts = xs_opts, xf_opts = xf_opts,
    env = attr(formula_metadata$formula, ".Environment")
  )
  names(special_component_metadata) <- special_predictor_names
  special_component_metadata
}

transform_formula_chr <- function(formula_metadata, special_components_details) {

  replace_component_call <- function(rhs_string_formula, component_details) {
    sub(component_details$call, component_details$new_call, rhs_string_formula, fixed = TRUE)
  }

  transformed_rhs <- purrr::reduce(
    special_components_details,
    replace_component_call,
    .init = formula_metadata$rhs
  )

  sprintf("%s ~ %s", formula_metadata$lhs, transformed_rhs)
}

correct_improved_components <- function(special_components_details, transformed_variables) {

  use_untransformed <- function(special_component_metadata) {
    !(special_component_metadata$var %in% transformed_variables)
  }

  use_bare_call <- function(special_component_metadata) {
    special_component_metadata$new_call <- special_component_metadata$var
    special_component_metadata
  }

  special_components_details %>%
    purrr::map_if(use_untransformed, use_bare_call)

}

add_special_to_predictor <- function(predictor, class) {
  if (!(class %in% c("numeric", "integer", "factor"))) {
    stop("Wrong class passed.")
  }
  if (class == "factor") {
    sprintf("xf(%s)", predictor)
  } else {
    sprintf("xs(%s)", predictor)
  }
}

build_predictor_based_formula <- function(response, predictors, classes, form = "additive") {
  if (form == "additive") {
    collapse = " + "
  } else {
    collapse = " * "
  }
  rhs <- purrr::map2_chr(
    predictors, classes, add_special_to_predictor) %>%
    paste(collapse = collapse)

  sprintf("%s ~ %s", response, rhs)
}

get_predictors_classes <- function(data) {
  purrr::map_chr(1:ncol(data), function(x) class(data[, x]))
}

try_get <- function(possible) {
  possible_response <- try(possible, silent = TRUE)
  if (class(possible_response) != "try-error") {
    possible_response
  } else {
    NULL
  }
}

get_model_data <- function(model, data, env = parent.frame()) {
  if (is.null(data)) {
    data <- try_get(eval(getCall(model)$data, envir = env))
  }
  if (is.null(data)) {
    stop("Data must be provided.")
  }
  data
}

get_model_response <- function(model, data, response) {
  if (is.null(response)) {
    response <- try_get(all.vars(model$terms[[2]]))
  }
  if (!is.null(response)) {
    response_in_data <- response %in% colnames(data)
    if (!all(response_in_data)) {
      stop("Response not found in data")
    }
    response <- response[response_in_data]
  } else {
    stop("Cannot extract model lhs")
  }
  response
}

get_model_lhs <- function(model, lhs) {
  if (is.null(lhs)) {
    lhs <- try_get(deparse(model$terms[[2]]))
  }
  if (is.null(lhs)) {
    lhs <- try_get(colnames(model.frame(model))[1])
  }
  if (is.null(lhs)) {
    stop("Cannot extract model lhs")
  }
  lhs
}

get_model_predictors <- function(model, data, predictors, response) {
  if (is.null(predictors)) {
    predictors <- try_get(all.vars(model$terms[[3]]))
  }
  if (!is.null(predictors)) {
    predictors_in_data <- predictors %in% colnames(data)
    if (!all(predictors_in_data)) {
      stop("Not all predictors found in data")
    }
    predictors <- predictors[predictors_in_data]
  } else {
    predictors <- setdiff(colnames(data), response)
  }
  predictors
}

get_model_type <- function(model, data) {
  response <- get_model_response(model, data, NULL)

  if (inherits(data[[response]], "factor")) {
    type <- "classification"
  } else if (inherits(data[[response]], "integer") && (length(unique(data[[response]])) <= 2)) {
    type <- "classification"
  } else {
    type <- "regression"
  }
  type
}

get_model_family <- function(model, family, type) {

  model_family <- try_get(
    match.fun(eval(getCall(model)$family)$family)
  )

  if (is.null(model_family)) {
    if (type == "classification") {
      model_family <- match.fun("binomial")
    } else {
      if (is.character(family)) {
        model_family <- match.fun(family)
        family_name <- family
      } else {
        model_family <- match.fun(family$family)
        family_name <- family$family
      }
      message(sprintf("Cannot extract model family. Use %s.", family_name))
    }
  }

  model_family
}

get_model_link <- function(model, link, type) {
  model_link <- try_get(
    eval(getCall(model)$family)$link
  )

  if (is.null(model_link)) {
    if (type == "classification") {
      model_link <- "logit"
    } else {
      if (is.null(model_link)) {
        message(sprintf("Cannot extract model link. Use %s.", link))
        model_link <- link
      }
    }
  }

  model_link
}

get_formula_lhs <- function(formula) {
  deparse(formula[[2]], width.cutoff = 500)
}

get_formula_rhs <- function(formula) {
  gsub("\\s+", " ", trimws(paste0(deparse(formula[[3]]), collapse = "")))
}

get_formula_predictors <- function(formula, data, predictors, response) {
  if (is.null(predictors)) {
    predictors <- all.vars(formula[[3]])
  }
  if (!is.null(predictors)) {
    predictors_in_data <- predictors %in% colnames(data)
    if (!all(predictors_in_data)) {
      stop("Not all predictors found in data")
    }
    predictors <- predictors[predictors_in_data]
  } else {
    predictors <- setdiff(colnames(data), response)
  }
  predictors
}

get_formula_response <- function(formula, data, response) {
  if (is.null(response)) {
    response <- try_get(all.vars(formula[[2]]))
  }
  if (!is.null(response)) {
    response_in_data <- response %in% colnames(data)
    if (!all(response_in_data)) {
      stop("Response not found in data")
    }
    response <- response[response_in_data]
  } else {
    stop("Cannot extract formula lhs")
  }
  response
}

add_specials_to_formula <- function(formula_components, data, omit = c("xs", "xf", "+", "*", ":", "~")) {
  if (length(formula_components) == 1 && !(as.character(formula_components)[1] %in% omit)) {
    if (class(data[[as.character(formula_components)]]) == "factor") {
      template <- quote(xf(var))
    } else {
      template <- quote(xs(var))
    }
    template[[2]] <- formula_components
    formula_components <- template
    formula_components <- formula_components
  } else if (length(formula_components) >= 3) {
    if (as.character(formula_components[[1]]) %in% omit[-c(1, 2)]) {
      for (i in seq_along(formula_components)) {
        formula_components[[i]] <- add_specials_to_formula(formula_components[[i]], data, omit)
      }
    }
  }
  formula_components
}

transformed_formula_object <- function(formula_metadata, blackbox, data, family, xs_opts, xf_opts, compare_stat) {

  special_components_details <- collect_specials_metadata(formula_metadata, xs_opts, xf_opts)
  transitions <- get_transitions_outcome(formula_metadata, special_components_details, blackbox, data,
                                         family, compare_stat)

  quantitative_transitions <- transitions$quantitative %>%
    purrr::keep(~ !(is.null(.$transition_outcome)))
  xs_functions <- purrr::map2(quantitative_transitions, names(quantitative_transitions), build_xs_function) %>%
    purrr::set_names(names(quantitative_transitions))
  xs <- function(variable) {
    var_name <- deparse(substitute(variable))
    xs_functions[[var_name]](variable)
  }

  qualitative_transitions <- transitions$qualitative %>%
    purrr::keep(~ !(is.null(.$transition_outcome)))
  xf_functions <- purrr::map2(qualitative_transitions, names(qualitative_transitions), build_xf_function) %>%
    purrr::set_names(names(qualitative_transitions))
  xf <- function(variable) {
    var_name <- deparse(substitute(variable))
    xf_functions[[var_name]](variable)
  }

  transformed_formula_env <- attr(formula_metadata$formula, ".Environment")
  transformed_formula_env$xs <- xs
  transformed_formula_env$xf <- xf
  transformed_formula_env$xs_functions <- xs_functions
  transformed_formula_env$xf_functions <- xf_functions
  transformed_formula_env$quantitative_transitions <- quantitative_transitions
  transformed_formula_env$qualitative_transitions <- qualitative_transitions
  transformed_formula_env$response <- formula_metadata$response

  variables_to_transform <- c(names(quantitative_transitions), names(qualitative_transitions))
  special_components_details <- correct_improved_components(special_components_details, variables_to_transform)
  transformed_formula_string <- transform_formula_chr(formula_metadata, special_components_details)
  as.formula(transformed_formula_string, env = transformed_formula_env)
}
