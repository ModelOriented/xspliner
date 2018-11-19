# mgcv_approx.R

#" Plot fitted spline function. (!this will be considered while defining plot)
#'
#' It plots fitted spline function with ggplot package.
#' @param spline_function Extracted spline function with 'data' attribute.
#' @param add If TRUE the plot is added to another one specified by \code{base_plot} parameter.
#' @param base_plot ggplot object to which function should add graph.
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' data <- data.frame(x = x, y = y)
#' gam_model <- xspline_approx(data, bs = "tp")
#' spline <- xspline_function(gam_model)
#' xspline_plot(spline)
#' p <- ggplot(data, aes(x = x, y = y)) + geom_point()
#' xspline_plot(spline, add = TRUE, p)
#' @export
xspline_plot <- function(spline_function, add = FALSE, base_plot = NULL) {
  plot_range <- attr(spline_function, "variable_range")
  plot_data <- data.frame(x_val = seq(plot_range[1], plot_range[2], by = 0.01)) %>%
    dplyr::mutate(y_val = spline_function(x_val))

  if (add) {
    if (is.null(base_plot)) {
      stop("base_plot must be provided")
    }
    base_plot +
      ggplot2::geom_line(data = plot_data, ggplot2::aes(x = x_val, y = y_val), inherit.aes = FALSE)
  } else {
    ggplot2::ggplot(data = plot_data, ggplot2::aes(x = x_val, y = y_val)) +
      ggplot2::geom_line()
  }
}

# model_helpers.R

#" Determine variable transformation. (!consider while automatic approx decision, along with below functions)
#'
#' It builds two glm models with single predictor variable.
#' The first one is just simple glm model with predictor variable as is.
#' The second one transforms predictor variable with estimated spline function on the main model response.
#' @param model Output of \link{DALEX::explain} function.
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
get_pdp_spline_improvement <- function(model, data, pred_var, response_var, spline_pkg = "gam", type = "pdp", monotonic = NULL, ...) {
  if (!is.character(response_var) || !is.character(pred_var))
    stop("Both response_var and pred_var should be character!")
  if (!type %in% c("pdp", "ale"))
    stop("type can be only 'pdp' or 'ale'!")

  transform <- I
  base_output <- list(transform = transform,
                      spline_plot = NULL,
                      aic = NULL)

  model_variable_response  <- variable_response(model, variable = pred_var, type = type, which.class = 2, prob = TRUE)
  p <- plot(model_variable_response)

  if ("factorMerger" %in% class(model_variable_response)) {
    merged_factors <- factorMerger::getOptimalPartitionDf(model_variable_response, "GIC", 3)
    avg_group_response <- data.frame(x = model_variable_response$factor, y = model_variable_response$response) %>%
      dplyr::group_by(x) %>%
      dplyr::summarise(y = mean(y)) %>%
      dplyr::left_join(merged_factors, by = c('x' = 'orig')) %>%
      dplyr::group_by(pred) %>%
      dplyr::mutate(y = mean(y)) %>%
      dplyr::ungroup() %>%
      dplyr::select(x, y)

    transform <- function(val) lapply(val, function(val) {avg_group_response[avg_group_response$x == val, ]$y}) %>%
      unlist()

    return(
      list(transform = transform,
           transform_plot = p,
           aic = NULL)
    )
  }

  tryCatch({
    xspline_approx_model <- function(data) xspline_approx(data, type = spline_pkg, monotonicity = monotonic, ...)

    spline_on_model_variable_response <- xspline_approx_model(model_variable_response)
    spline <- xspline_function(spline_on_model_variable_response)
    # p <- plot(model_variable_response)
    # xspline_plot(spline, add = TRUE, p)
    model_data <- data.frame(x = data[[pred_var]], y = data[[response_var]])

    glm_on_spline_approx <- glm(y~spline(x), data = model_data, family = "binomial")

    glm_on_raw_variable <- glm(y~x, data = model_data, family = "binomial")

    if (glm_on_spline_approx$aic < glm_on_raw_variable$aic)
      transform <- spline

    return(
      list(transform = transform,
           transform_plot = xspline_plot(spline, add = TRUE, p),
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

