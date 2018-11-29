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
#' @param predictors Predictor values that should be used in final model.
#' @param data Training data of \code{model}.
#' @param env Environment in which optional variables passed into parameters are stored.
#' @param factor_opts Formula parameters used for factor variable transformatoins inherited from factorMerger package.
#' @param numeric_opts Predictive model response method and approximation parameters used for numeric
#'  variables transformation. See vignette("xspliner") for details.
#'
#' @rdname xspline
#' @export
xspline.default <- function(model, lhs = NULL, response = NULL, predictors = NULL, data = NULL, env = parent.frame(),
                            factor_opts = factor_opts_default, numeric_opts = numeric_opts_default, ...) {
  data <- get_model_data(model, data, env)
  lhs <- get_model_lhs(model, lhs)
  predictors <- get_model_predictors(model, data, predictors, get_model_response(model, data, response))
  classes <- get_predictors_classes(data[, predictors])

  formula <- as.formula(
    build_formula(lhs, predictors, classes, factor_opts, numeric_opts),
    env = env)
  build_xspliner(formula, model, data, env = env, ...)
}

#' @param formula xspliner-specific formula object. Check vignette("xspliner") for more details.
#' @param exact If TRUE, exact formula call is used. If not all formula variables are altered.
#' @rdname xspline
#' @export
xspline.formula <- function(formula, model, data = NULL, exact = TRUE, env = parent.frame(), ...) {
  data <- get_model_data(model, data, env)

  formula_lhs <- get_formula_lhs(formula)
  model_lhs <- get_model_lhs(model, NULL)
  if (model_lhs != formula_lhs) {
    warning("Model and formula lhs's must be the same. Using lhs from model.")
    formula[[2]] <- model$terms[[2]]
  }

  model_predictors <- get_model_predictors(model, data, NULL, get_model_response(model, data, NULL))
  if (get_formula_rhs(formula) == ".") {
    lhs <- get_formula_lhs(formula)
    xspline.default(model, lhs, NULL, model_predictors, data, env = env, ...)
  } else {
    formula_predictors <- get_formula_predictors(formula, data, NULL, get_formula_response(formula, data, NULL))
    if (!(all(formula_predictors %in% model_predictors))) {
      stop("Not all variables from formula are included in model.")
    }
    if (exact) {
      build_xspliner(formula, model, data, env = env, ...)
    } else {
      lhs <- get_formula_lhs(formula)
      xspline.default(model, lhs, NULL, formula_predictors, data, env = env, ...)
    }
  }
}

#' @param explainer Object of class 'explainer' (see \link{DALEX} package).
#' @rdname xspline
#' @export
xspline.explainer <- function(explainer, env = parent.frame(), ...) {
  xspline.default(explainer$model, NULL, NULL, NULL, env = env, ...)
}

#' Helper function for building GLM object with transformed variables.
#'
#' @param formula xspliner-specific formula object. Check vignette("xspliner") for more details.
#' @param model Predictive model. Basic model used for extracting predictors transformation.
#' @param data Training data of \code{model}.
#' @param link Link function that should be used in final model. The passed is used when cannot be extracted from
#'   model. By default 'identity'. See \link{stats::family} for possibilities.
#' @param family Family of response variable that should be used in final model. The passed is used when cannot
#'   be extracted from model. By default 'gaussian'. See \link{stats::family} for possibilities.
#' @param env Environment in which optional variables passed into parameters are stored.
#' @param alter Specifies if each variable transformation should be compared with bare variable usage.
#'   List of the form: list(numeric = type, factor = type), type one of c('always', 'auto', 'never).
#'   'auto' option available only for qualitative variable. The better model is specified by \code{compare_stat} parameter then.
#' @param compare_stat Function of linear model (lm function output). Statistic that measures if linear model is better that transformed one.
#'   See \link{stats}.
#'
build_xspliner <- function(formula, model, data, link = "identity", family = "gaussian", env = parent.frame(),
                           alter = list(numeric = 'always', factor = 'never'), compare_stat = r_squared) {
  formula_environment <- new.env(parent = env)
  attr(formula, ".Environment") <- formula_environment
  formula_details <- get_formula_details(formula, extract_formula_var_names(formula, data))
  cleared_formula <- transformed_formula_object(formula_details, model, data, alter, compare_stat)
  type <- get_model_type(model, data)
  model_family <- get_model_family(model, family, type)
  model_link <- get_model_link(model, link, type)
  gam_model <- glm(cleared_formula, data = data, family = model_family(link = model_link))
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
