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
#' @param lhs Left-hand side of model formula. Can be transformed response.
#' @param response Name of response variable of \code{model}.
#' @param predictors Predictor values that should be used in final model.
#' @param data Training data of \code{model}.
#' @param form Can be 'additive' (default) or 'multiplicative'. Specifies formula form in final model.
#' @param env Environment in which optional variables passed into parameters are stored.
#'  variables transformation. See vignette("xspliner") for details.
#'
#' @rdname xspline
#' @export
xspline.default <- function(model, lhs = NULL, response = NULL, predictors = NULL, data = NULL,
                            form = "additive", env = parent.frame(), ...) {
  data <- get_model_data(model, data, env)
  lhs <- get_model_lhs(model, lhs)
  predictors <- get_model_predictors(model, data, predictors, get_model_response(model, data, response))
  classes <- get_predictors_classes(data[, predictors])

  formula <- as.formula(
    build_predictor_based_formula(lhs, predictors, classes, form),
    env = env)
  build_xspliner(formula, model, data, env = env, ...)
}

#' @param formula xspliner-specific formula object. Check vignette("xspliner") for more details.
#' @param consider One of \code{c("specials", "all")}. If "specials", only components with xs or xf
#'   call are considered in transition.
#' @rdname xspline
#' @export
xspline.formula <- function(formula, model, data = NULL, consider = "specials", env = parent.frame(), ...) {
  data <- get_model_data(model, data, env)
  formula_lhs <- get_formula_lhs(formula)
  model_lhs <- get_model_lhs(model, NULL)
  if (model_lhs != formula_lhs) {
    message("Model and formula lhs's must be the same. Using lhs from model.")
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
    if (consider == "specials") {
      build_xspliner(formula, model, data, env = env, ...)
    } else {
      formula[[3]] <- add_specials_to_formula(formula[[3]], data)
      xspline.formula(formula, model, data = NULL, consider = "specials", env = env, ...)
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
#' @param xf_opts Formula parameters used for factor variable transformatoins inherited from factorMerger package.
#' @param xs_opts Predictive model response method and approximation parameters used for quantitative.
#' @param link Link function that should be used in final model. The passed is used when cannot be extracted from
#'   model. By default 'identity'. See \link{stats::family} for possibilities.
#' @param family Family of response variable that should be used in final model. The passed is used when cannot
#'   be extracted from model. By default 'gaussian'. See \link{stats::family} for possibilities.
#' @param env Environment in which optional variables passed into parameters are stored.
#' @param compare_stat Function of linear model (lm function output). Statistic that measures if linear model is better that transformed one.
#'   See \link{stats}.
#'
build_xspliner <- function(formula, model, data, xf_opts = xf_opts_default, xs_opts = xs_opts_default,
                           link = "identity", family = "gaussian", env = parent.frame(), compare_stat = aic) {
  formula_environment <- new.env(parent = env)
  attr(formula, ".Environment") <- formula_environment
  formula_metadata <- get_formula_metadata(formula, extract_formula_var_names(formula, data))
  type <- get_model_type(model, data)
  model_family <- get_model_family(model, family, type)
  model_link <- get_model_link(model, link, type)
  family <- model_family(link = model_link)
  cleared_formula <- transformed_formula_object(formula_metadata, model, data, family, xs_opts, xf_opts, compare_stat)
  glm_model <- glm(cleared_formula, data = data, family = family)
  environment(glm_model) <- attr(cleared_formula, ".Environment")
  class(glm_model) <- c("xspliner", class(glm_model))
  glm_model
}

#' Default parameters for transition methods
#'
#' While constructing formula interpreted by xspliner package, some parameters may be specified within xs(..) or xf(..) symbols.
#' Below are default parameters. See details in \code{vignette("xspliner")}
#'
#' @export
xf_opts_default = list(
  effect = list(type = "fM"),
  transition = list(alter = "newer", stat = "GIC", value = 3)
)

#' @rdname xf_opts_default
#' @export
xs_opts_default = list(
  effect = list(type = "pdp"),
  transition = list(alter = "always", monotonic = "not")
)

#' Statistics used for better linear model selection
#'
#' Used as \code{compare_stat} parameter in \code{xspline} method.
#' Each function has attribute "higher-better".
#' If "higher-better" is TRUE then model with higher statistic value is treated as better one.
#'
#' @param glm_model Linear model - \code{glm} function output.
#' @param family Family used for fitting the model.
#' @name stats
NULL

#' Calculate AIC for glm model.
#'
#' @rdname stats
#' @export
aic <- function(glm_model) {
  summary(glm_model)$aic
}
attr(aic, "higher-better") <- FALSE

#' Calculate Hosmer-Lemeshow Goodness of Fit for glm model.
#'
#' @rdname stats
#' @export
hoslem <- function(glm_model) {
  if (glm_model$family$family != "binomial") {
    stop("Not classification model.")
  }
  ResourceSelection::hoslem.test(glm_model$model[, 1], fitted(glm_model))$statistic
}
attr(hoslem, "higher-better") <- TRUE
