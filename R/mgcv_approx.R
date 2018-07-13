#" Approximate spline on data
#'
#' It aproximates data with spline function by fitting GAM model.
#' @param data Data with \code{x} and \code{y} variables.
#' @param ... Other arguments passed to \link{mgcv::s} function.
#' @return
#' Object of class "gam". See \link{mgcv::gamObject}
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' xspline_approx_gam(data.frame(x = x, y = y), bs = "tp")
#' @export
xspline_approx_gam <- function(data, ...) {
  if (!all(c("x", "y") %in% colnames(data))) {
    stop("data must have 'x' and 'y' variable")
  }
  x <- data$x
  y <- data$y
  s <- mgcv::s
  mgcv::gam(y ~ s(x, ...))
}

#" Approximate monotonic spline on data
#'
#' It aproximates data with spline function by Penalized Constrained Least Squares Fitting.
#' @param data Data with \code{x} and \code{y} variables.
#' @param ... Other arguments passed to \link{mgcv::s} function.
#' @param increasing Should be approximation monotonic
#' @return
#' Object of class "gam". See \link{mgcv::gamObject}
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' xspline_approx_gam_mon(data.frame(x = x, y = y), bs = "tp", increasing = FALSE)
#' @export
xspline_approx_gam_mon <- function(data, increasing = TRUE, ...) {
  x <- data$x
  y <- data$y
  s <- mgcv::s
  G <- mgcv::gam(y ~ s(x, ...), fit = FALSE)
  contraint_sign <- if (increasing) 1 else -1

  gam_init <- gam(G = G)

  ## generate constraints, by finite differencing
  ## using predict.gam ....
  eps <- 1e-7
  x_range <- range(x)
  diff_grid_0 <- diff_grid_1 <- data.frame(x = seq(x_range[1], x_range[2], length.out = 100))
  diff_grid_1$x <- diff_grid_1$x + eps
  spline_vals_on_interv_start <- predict(gam_init, newdata = diff_grid_0, type = "lpmatrix")
  spline_vals_on_interv_end <- predict(gam_init, newdata = diff_grid_1, type = "lpmatrix")
  x_var_constraints <- contraint_sign * (spline_vals_on_interv_end - spline_vals_on_interv_start) / eps ## Xx %*% coef(b) must be positive
  G$Ain <- x_var_constraints # inequality constraint matrix
  G$bin <- rep(0, nrow(G$Ain)) # rhs vecctor for constraints
  G$C = matrix(0, 0, ncol(G$X)) # equality constraints (0 means lack of contraint)
  G$sp <- b$sp # smoothing parameter array
  G$p <- coef(b) # initial coeficients array
  G$off <- G$off - 1 # to match what pcls is expecting (moving index of penalty matrix)
  ## force inital parameters to meet constraint
  G$p[-1] <- 0
  coeffs <- pcls(G) ## constrained fit
  gam_init$coefficients <- coeffs
  gam_init
}


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


#" Approximate monotonic spline on data
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

#" Extract fitted spline function.
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
  if (class(model) == gam) {
    return(xspline_function_gam(fitted_model, ...))
  } else {
    return(xspline_function_rms(fitted_model))
  }
}

#" Plot fitted spline function.
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
