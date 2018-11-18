#' @export
xp_gam <- function(formula, blackbox, data = model.frame(blackbox), env = parent.frame()) {
  attr(formula, ".Environment") <- env
  formula_details <- get_formula_details(formula, extract_formula_var_names(formula, data))
  cleared_formula <- transformed_formula_object(formula_details, blackbox, data)
  mgcv::gam(cleared_formula, data = data)
}

#' @export
xp_gam_predict <- function(xp_gam_model, newdata) {
  mgcv::predict.gam(xp_gam_model, newdata = newdata)
}
