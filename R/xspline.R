#' Builds predictive model based GLM.
#'
#' See details in \code{vignette("xspliner")}.
#'
#' @param object Predictive model, formula or explainer (see \link{DALEX}) object.
#' @param ... Other arguments passed to \code{xspline} methods or \link{build_xspliner}.
#'
#' @return GLM object of class 'xspliner'.
#' @export
xspline <- function(object, ...) {
  UseMethod("xspline", object)
}

#' @param model Predictive model. Basic model used for extracting predictors transformation.
#' @param response Name of response variable of \code{model}.
#' @param data Training data of \code{model}.
#' @param env Environment in which optional variables passed into parameters are stored.
#' @param factor_opts Formula parameters used for factor variable transformatoins inherited from factorMerger package.
#' @param numeric_opts Predictive model response method and approximation parameters used for numeric
#'  variables transformation. See vignette("xspliner") for details.
#'
#' @rdname xspline
#' @export
xspline.default <- function(model, response = NULL, data = NULL, env = parent.frame(),
                            factor_opts = factor_opts_default, numeric_opts = numeric_opts_default, ...) {
  data <- get_model_data(model, data)
  response <- get_model_response(model, response)
  predictors <- setdiff(colnames(data), response)
  classes <- get_df_classes(data[, predictors])
  formula <- as.formula(
    build_formula(response, predictors, classes, factor_opts, numeric_opts),
    env = env)

  build_xspliner(formula, model, data, env = env, ...)
}

#' @param formula xspliner-specific formula object. Check vignette("xspliner") for more details.
#' @rdname xspline
#' @export
xspline.formula <- function(formula, model, data = NULL, env = parent.frame(), ...) {
  data <- get_model_data(model, data)
  if (get_formula_rhs(formula) == ".") {
    response <- get_formula_lhs(formula)
    xspline(model, response, data, env = env, ...)
  } else {
    build_xspliner(formula, model, data, env = env, ...)
  }
}

#' @param explainer Object of class 'explainer' (see \link{DALEX} package).
#' @rdname xspline
#' @export
xspline.explainer <- function(explainer, env = parent.frame(), ...) {
  xspline(explainer$model, explainer$y, explainer$data, env = env, ...)
}

#' Helper function for building GLM object with transformed variables.
#'
#' @param formula xspliner-specific formula object. Check vignette("xspliner") for more details.
#' @param model Predictive model. Basic model used for extracting predictors transformation.
#' @param data Training data of \code{model}.
#' @param env Environment in which optional variables passed into parameters are stored.
#' @param alter Specifies if each variable transformation should be compared with bare variable usage.
#'   List of the form: list(numeric = type, factor = type), type one of c('always', 'auto', 'never).
#'   'auto' option available only for qualitative variable. The better model is specified by \code{compare_stat} parameter then.
#' @param compare_stat Function of linear model (lm function output). Statistic that measures if linear model is better that transformed one.
#'   See \link{stats}.
#'
build_xspliner <- function(formula, model, data, env = parent.frame(),
                           alter = list(numeric = 'always', factor = 'never'), compare_stat = r_squared) {
  formula_environment <- new.env(parent = env)
  attr(formula, ".Environment") <- formula_environment
  formula_details <- get_formula_details(formula, extract_formula_var_names(formula, data))
  cleared_formula <- transformed_formula_object(formula_details, model, data, alter, compare_stat)
  gam_model <- mgcv::gam(cleared_formula, data = data)
  environment(gam_model$formula) <- environment(gam_model$pred.formula) <- environment(gam_model$terms) <-
    environment(gam_model$pterms) <- environment(gam_model) <- environment(attr(gam_model$model, "terms")) <-
    attr(cleared_formula, ".Environment")
  class(gam_model) <- c("xspliner", class(gam_model))
  gam_model
}

#' Statistics used for better linear model selection
#'
#' Used as \code{compare_stat} parameter in \code{xspline} method.
#' Each function has attribute "higher-better".
#' If "higher-better" is TRUE then model with higher statistic value is treated as better one.
#'
#' @param lm_model Linear model - \code{lm} function output.
#' @name stats
NULL

#' Calculate R Squared for linear model.
#'
#' @rdname stats
#' @export
r_squared <- function(lm_model) {
  summary(lm_model)$r.squared
}
attr(r_squared, "higher-better") <- TRUE
