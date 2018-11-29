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
    data <- try_get(eval(stats::getCall(model)$data, envir = env))
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
    match.fun(eval(stats::getCall(model)$family)$family)
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
    eval(stats::getCall(model)$family)$link
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
