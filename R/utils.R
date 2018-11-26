get_df_classes <- function(data) {
  purrr::map_chr(data, class)
}

try_get <- function(value, possible) {
  possible_response <- try(possible, silent = TRUE)
  if (class(possible_response) != "try-error") {
    response <- possible_response
  } else {
    NULL
  }
}

get_model_data <- function(model, data) {
  if (is.null(data)) {
    data <- try_get(data, model.frame(model))
  }
  if (is.null(data)) {
    stop("Data must be provided.")
  }
  data
}

get_model_response <- function(model, response) {
  if (is.null(response)) {
    response <- try_get(response, deparse(boston.rf$terms[[2]]))
  }
  if (is.null(response)) {
    response <- try_get(response, colnames(model.frame(model))[1])
  }
  if (is.null(response)) {
    stop("Data must be provided.")
  }
  response
}

get_formula_lhs <- function(formula) {
  deparse(formula[[2]], width.cutoff = 500)
}

get_formula_rhs <- function(formula) {
  gsub("\\s+", " ", trimws(paste0(deparse(formula[[3]]), collapse = "")))
}
