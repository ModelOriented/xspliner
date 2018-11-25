get_df_classes <- function(data) {
  purrr::map_chr(data, class)
}

get_model_data <- function(model, data) {
  if (is.null(data)) {
    possible_data <- try(model.frame(model), silent = TRUE)
    if (class(possible_data) != "try-error") {
      data <- possible_data
    }
  }
  if (is.null(data)) {
    stop("Data must be provided.")
  }
  data
}

get_model_response <- function(model, response) {
  if (is.null(response)) {
    possible_response <- try(colnames(model.frame(model))[1], silent = TRUE)
    if (class(possible_response) != "try-error") {
      response <- possible_response
    }
  }
  if (is.null(response)) {
    stop("Data must be provided.")
  }
  response
}

get_formula_lhs <- function(formula, data) {
  # (todo) should work on `y ~ .` formulas: extract_formula_var_names(formula, data)[1]
  all.vars(formula)[1]
}

get_formula_rhs <- function(formula) {
  paste(deparse(formula[[3]]), collapse = "")
}
