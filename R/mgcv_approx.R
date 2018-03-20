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
#' xspline_approx(data.frame(x = x, y = y), bs = "tp")
#' @export
xspline_approx <- function(data, ...) {
  if (!all(c("x", "y") %in% colnames(data))) {
    stop("data must have 'x' and 'y' variable")
  }
  x <- data$x
  y <- data$y
  s <- mgcv::s
  mgcv::gam(y ~ s(x, ...))
}

#" Extract fitted spline function.
#'
#' It extracts fitted spline function by \link{xspline_approx}.
#' @param fitted_gam Fitted gam model approximating data.
#' @param ... Other arguments passed to \link{mgcv::predict.gam} method.
#' @return
#' Function of one variable.
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' gam_model <- xspline_approx(data.frame(x = x, y = y), bs = "tp")
#' spline <- xspline_function(gam_model)
#' plot(spline)
#' @export
xspline_function <- function(fitted_gam, ...) {
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
