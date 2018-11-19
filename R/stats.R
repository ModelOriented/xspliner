#' @export
r_squared <- function(lm_model) {
  summary(lm_model)$r.squared
}
