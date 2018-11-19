# mgcv_approx.R

#" Approximate spline on data
#'
#' It aproximates data with spline function by fitting GAM model with rms package.
#' @param data Data with \code{x} and \code{y} variables.
#' @param ... Other arguments passed to \link{mgcv::s} function.
#' @return
#' Object of class "rms".
#' @examples
#' x <- sort(rnorm(20, 0, 5))
#' y <- x^3 + rnorm(20, 0, 1)
#' rms_model <- xspline_approx_rms(data.frame(x = x, y = y))
#' @export
xspline_approx_rms <- function(data) {
  if (!all(c("x", "y") %in% colnames(data))) {
    stop("data must have 'x' and 'y' variable")
  }
  x <- data$x
  y <- data$y
  if (!is.null(getOption("ols_knots_number"))) {
    model <- rms::ols(y ~ rms::rcs(x, getOption("ols_knots_number")))
  } else {
    model <- rms::ols(y ~ rms::rcs(x))
  }

  model$model <- data
  model
}


#" Approximate monotonic spline on data (!consider)
#'
#' It aproximates data with spline function by Penalized Constrained Least Squares Fitting.
#' @param data Data with \code{x} and \code{y} variables.
#' @param type Method for spline estimation. Possible values: 'gam' or 'rms'.
#' @param monotonicity Should spline be 'increasing', 'decreasing' or non-monotonic (NULL). Supporteed for 'gam' type.
#' @param ... Other arguments passed to \link{mgcv::s} function.
#' @return
#' Object of class "gam". See \link{mgcv::gamObject}
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' xspline_approx(data.frame(x = x, y = y), bs = "tp", monotonicity = "increasing")
#' @export
xspline_approx <- function(data, type = "gam", monotonicity = NULL, ...) {
  if (!type %in% c("gam", "rms"))
    stop("Type can have only 'gam' or 'rms' values")
  if (type == 'rms' && !is.null(monotonicity))
    stop("Monotonic splines are supported only for 'gam' type")
  spline_args <- list(...)
  if (type == "gam" && !is.null(monotonicity)) {
    if (!(monotonicity %in% c("increasing", "decreasing") || is.null(monotonicity)))
      stop("'monotonicity' can be just 'increasing', 'decreasing' or NULL")
    return(xspline_approx_gam_mon(data, monotonicity == "increasing", ...))
  } else if (type == "gam" && is.null(monotonicity)) {
    return(xspline_approx_gam(data, ...))
  } else {
    if (length(spline_args) > 0)
      warning("You can pas parameters for spline just for 'gam' type. Parameters ignored")
    return(xspline_approx_rms(data))
  }
}

#" Extract fitted spline function. (!consider with DALEX integration)
#'
#' It extracts fitted spline function by \link{xspline_approx_gam}.
#' @param fitted_gam Fitted gam model approximating data.
#' @param ... Other arguments passed to \link{mgcv::predict.gam} method.
#' @return
#' Function of one variable.
#' @examples
#' x <- sort(rnorm(20, 0, 5))
#' y <- x^3 + rnorm(20, 0, 1)
#' gam_model <- xspline_approx_gam(data.frame(x = x, y = y), bs = "tp")
#' spline <- xspline_function_gam(gam_model)
#' plot(spline, min(x), max(x))
#'
#' set.seed(123)
#' x <- sort(rnorm(40, 0, 5))
#' y <- sin(x) + log(abs(x)) + rnorm(40, 0, 1)
#' plot(x, y)
#' gam_model_mon <- xspline_approx_gam_mon(data.frame(x = x, y = y), increasing = TRUE, bs = "tp")
#' spline_mon <- xspline_function_gam(gam_model_mon)
#' gam_model <- xspline_approx_gam(data.frame(x = x, y = y), bs = "tp")
#'spline <- xspline_function_gam(gam_model)
# plot(spline_mon, min(x), max(x), add = TRUE)
# plot(spline, min(x), max(x), add = TRUE)
#' @export
xspline_function_gam <- function(fitted_gam, ...) {
  if (!("gam" %in% class(fitted_gam))) {
    stop("fitted_gam should be of 'gam' class")
  }
  model <- fitted_gam
  spline <- function(val) {
    data <- data.frame(x = val)
    mgcv::predict.gam(model, data, ...)
  }
  attr(spline, "variable_range") <- range(model$model$x)
  spline
}
#" Extract fitted spline function for rms estimator.
#'
#' It extracts fitted spline function by \link{xspline_approx}.
#' @param fitted_gam Fitted gam model approximating data.
#' @param ... Other arguments passed to \link{mgcv::predict.gam} method.
#' @return
#' Function of one variable.
#' @examples
#' x <- sort(rnorm(20, 0, 5))
#' y <- x^3 + rnorm(20, 0, 1)
#' rms_model <- xspline_approx_rms(data.frame(x = x, y = y))
#' spline <- xspline_function_rms(rms_model)
#' plot(x, y)
#' plot(spline, min(x), max(x))
#' @rdname xspline_function_gam
#' @export
xspline_function_rms <- function(fitted_rms) {
  if (!("rms" %in% class(fitted_rms))) {
    stop("fitted_rms should be of 'rms' class")
  }
  model <- fitted_rms
  spline <- function(val) {
    data <- data.frame(x = val)
    predict(model, data)
  }
  attr(spline, "variable_range") <- range(model$model$x)
  spline
}

#' @rdname xspline_function_gam
xspline_function <- function(fitted_model, ...) {
  if (!any(c("gam", "rms") %in% class(fitted_model)))
    stop("fitted_model should be of class gam or rms")
  if (class(model) == "gam") {
    return(xspline_function_gam(fitted_model, ...))
  } else {
    return(xspline_function_rms(fitted_model))
  }
}

#" Plot fitted spline function. (!consider)
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

#" Determine variable transformation. (!consider while automatic approx decision)
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

### (!consider what object should be returned, maybe I don't need this) <- from new approach
xp_gam <- function(formula, blackbox, data = model.frame(blackbox)) {

  # some assumption checks

  xs_call <- list()
  xf_call <- list()

  formula_details <- get_formula_details(formula)

  # below should be named: prepare_formula_env
  for (xs_var in formula_details$xs_variables) {
    spline_params <- as.list(parse(text = formula_details$formula_labels[1])[[1]])
    spline_opts <- eval(spline_params$spline_opts)
    method_opts <- eval(spline_params$method_opts)

    bb_response_func <- do.call(get_bb_response, method_opts) # for example pdp curve
    bb_response_approx <- do.call(get_bb_response_approx, spline_opts) # estimation of pdp
    xs_call[[xs_var]] <- bb_response_approx
  }

  # find out how parent.frame works to get below correctly (maybe we don't want it):
  transformed_formula <- build_spline_formula(formula_details, env = c(xs_call, xf_call)) # we want "response ~ xs_call('x1')(x1) + xf_call('x2')(x2)"

  model <- glm(transformed_formula)

  list(
    xs_call = xs_call,
    xf_call = xf_call,
    model = model
  )
}
