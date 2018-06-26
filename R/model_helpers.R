#" Determine variable transformation.
#'
#' It builds two glm models with single predictor variable.
#' The first one is just simple glm model with predictor variable as is.
#' The second one transforms predictor variable with estimated slpine function on the main model response.
#' @param model Output of \link{DALEX::exlain} function.
#' @param data Data used for prediction
#' @param pred_var, response_var
#' @param spline_pkg Can be "gam" or "rms
#' @param type Can be pdp or ale
#' @param ... Other arguments passed to \link{xspline_approx_gam} method.
#' @return
#' List containing transformation function for respoonse_var, plot on the model and comparison with aic of fitted.
#' @examples
# data <- breakDown::HR_data
# logit <- function(x) exp(x)/(1+exp(x))
# HR_rf_model <- randomForest(factor(left)~., data = data, ntree = 100)
#
# explainer_rf  <- explain(HR_rf_model, data = data, y = data$left,
#                          predict_function = function(model, newdata, ...)
#                            predict(model, newdata, type = "prob")[,2])
#
# get_pdp_spline_improvement(explainer_rf, data, "satisfaction_level", "left", spline_pkg = "gam", type = "pdp", NULL)
#' @export
get_pdp_spline_improvement <- function(model, data, pred_var, response_var, spline_pkg = "gam", type = "pdp", ...) {
  if (!is.character(response_var) || !is.character(pred_var))
    stop("Both response_var and pred_var should be character!")
  if (!type %in% c("pdp", "ale"))
    stop("type can be only 'pdp' or 'ale'!")

  transform <- I
  base_output <- list(transform = transform,
                      spline_plot = NULL,
                      aic = NULL)

  if (class(data[[pred_var]]) %in% c("factor", "ordered"))
    return(base_output)

  tryCatch({
    model_variable_response  <- variable_response(model, variable = pred_var, type = type, which.class = 2, prob = TRUE)
    xspline_approx <- function(data) xspline_approx_gam(data, ...)
    xspline_function <- xspline_function_gam
    if (type == "rms") {
      xspline_approx <- xspline_approx_rms
      xspline_function <- xspline_function_rms
    }

    spline_on_model_variable_response <- xspline_approx(model_variable_response)
    spline <- xspline_function(spline_on_model_variable_response)
    # p <- plot(model_variable_response)
    # xspline_plot(spline, add = TRUE, p)
    model_data <- data.frame(x = data[[pred_var]], y = data[[response_var]])

    glm_on_spline_approx <- glm(y~spline(x), data = model_data, family = "binomial")

    glm_on_raw_variable <- glm(y~x, data = model_data, family = "binomial")

    if (glm_on_spline_approx$aic < glm_on_raw_variable$aic)
      transform <- spline

    p <- plot(model_variable_response)
    return(
      list(transform = transform,
           spline_plot = xspline_plot(spline, add = TRUE, p),
           aic = c(
             "spline" = glm_on_spline_approx$aic,
             "raw" = glm_on_raw_variable$aic))
    )
  }, error = function(e) base_output)
}

build_spline_formula <- function(model_name, data, response_var) {
  pred_vars <- setdiff(colnames(data), response_var)
  model_name <- rep(model_name, length(pred_vars))
  response_var_vec <- rep(response_var, length(pred_vars))
  rhs <- paste(
    sprintf("get_pdp_spline_improvement(%s, data, '%s', '%s')$transform(%s)", model_name, pred_vars, response_var_vec, pred_vars),
    collapse = " + ")
  as.formula(sprintf("%s ~ %s", response_var, rhs))
}

#" Builds glm model with transformed variables.
#'
#' Builds glm model with transformed variables.
#' @param model Output of \link{DALEX::exlain} function.
#' @param data Data used for prediction
#' @param response_var
#' @return
#' Glm model with transformedd variables with splines.
#' @example
#' HR_spline_model <- build_spline_model(explainer_rf, data, "left")
#' @export
build_spline_model <- function(model, data, response_var) {
  pred_vars <- setdiff(colnames(data), response_var)
  model_name <- deparse(substitute(model))
  f <- build_spline_formula(model_name, data, response_var)

  glm(f, data = data, family = "binomial")
}
