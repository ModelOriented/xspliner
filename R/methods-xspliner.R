#' Plot method for 'xspliner' model
#'
#' @param x Object of class 'xspliner'
#' @param variable_name Name of predictor that should be plotted.
#' @param plot_response If TRUE blackbox model response is drawn.
#' @param plot_approx If TRUE blackbox model response approcimation is drawn.
#' @param plot_data If TRUE raw data is drawn.
#' @param data Training data used for building \code{x} model. Required for plot_data option.
#' @param ... Another arguments passed into model specific method.
#'
#' @export
plot.xspliner <- function(x, variable_name = NULL, plot_response = TRUE, plot_approx = TRUE,
                    data = NULL, plot_data = FALSE, plot_deriv = TRUE, ...) {

  if (is.null(variable_name)) {
    class(x) <- "lm"
    plot(x, ...)
  } else if (!(variable_name %in% specials(x, "all"))) {
    stop("Variable wasn't transformed.")
  } else if (variable_name %in% specials(x, "qualitative")) {
    plot(transition(x, variable_name, "base"))
  } else {
    if (plot_data && is.null(data)) {
      message("You can plot data points only when data parameter is provided.")
      plot_data <- FALSE
    }

    to_plot <- c(plot_data, plot_response, plot_approx, plot_deriv)
    if (!any(to_plot)) {
      stop("You must specify at least one plot.")
    }
    transition_fun <- transition(x, variable_name, "function")

    if (plot_data) {
      plot_range <- range(data[, variable_name])
    } else {
      plot_range <- attr(transition_fun, "variable_range")
    }

    response_var <- environment(x)$response
    base_data <- data.frame(x = numeric(), y = numeric(), type = character())
    colnames(base_data) <- c(variable_name, response_var)
    color_values <- c("data" = "black", "response" = "red", "approximation" = "blue", "derivative" = "skyblue")

    if (plot_data) {
      data <- data[c(variable_name, response_var)]
      data$type <- "data"
      base_data <- rbind(base_data, data)
    }
    if (plot_response) {
      data <- transition(x, variable_name, "data")
      colnames(data)[colnames(data) == "yhat"] <- response_var
      names(color_values)[2] <- attr(data, "type")
      data$type <- attr(data, "type")
      base_data <- rbind(base_data, data)
    }
    if (plot_approx) {
      x_var <- seq(from = plot_range[1], to = plot_range[2], length.out = 50)
      y_var <- transition_fun(x_var)
      data <- data.frame(y_var, x_var)
      colnames(data) <- c(response_var, variable_name)
      data$type <- "approximation"
      base_data <- rbind(base_data, data)
    }
    if (plot_deriv) {
      eps <- (plot_range[2] - plot_range[1]) / 500
      x_var <- seq(from = plot_range[1], to = plot_range[2], length.out = 50)[-50]
      y_var <- (transition_fun(x_var + eps) - transition_fun(x_var)) / eps
      data <- data.frame(y_var, x_var)
      colnames(data) <- c(response_var, variable_name)
      if (sum(to_plot) == 1) {
        base_data <- data
        base_plot <- ggplot2::ggplot(data = base_data, ggplot2::aes_string(x = variable_name, y = response_var)) +
          ggplot2::geom_line() +
          ggplot2::labs(y = "Transformation derivative") +
          ggplot2::theme_minimal()
      } else {
        scaled <- data_linear_scaling(base_data[[response_var]], data[[response_var]])
        data[[response_var]] <- scaled$data
        data$type <- "derivative"
        base_data <- rbind(base_data, data)
        base_plot <- ggplot2::ggplot(data = base_data,
                                     ggplot2::aes_string(x = variable_name, y = response_var, colour = "type")) +
          ggplot2::geom_point(data = base_data[base_data$type == "data", ]) +
          ggplot2::geom_line(data = base_data[base_data$type != "data", ]) +
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(~ scaled$scaling * . + scaled$translation,
                                         name = "Transformation derivative")) +
          ggplot2::scale_color_manual(
            values = color_values) +
          ggplot2::labs(colour = "Plot type") +
          ggplot2::theme_minimal()
      }
    }
    base_plot
  }
}

#' Summary method for xspliner object
#'
#' @param object xspliner object
#' @param predictor precitor for xspliner model formula
#' @param ... Another arguments passed into model specific method.
#' @export
summary.xspliner <- function(object, predictor, ...) {
  if (missing(predictor)) {
    return(summary.glm(object, ...))
  }
  if (predictor %in% specials(object, "quantitative")) {
    return(mgcv::summary.gam(transition(object, predictor, "base"), ...))
  }
  if (predictor %in% specials(object, "qualitative")) {
    return(attributes(transition(object, predictor, "base"))$partition) # (todo) write own method
  }
  if (!(predictor %in% specials(object, "qualitative"))) {
    message("Variable was not transformed.")
    return(summary.glm(object, ...))
  }
}

#' Print method for xspliner object
#'
#' @param x xspliner object
#' @param predictor precitor for xsplner model formula
#' @param ... Another arguments passed into model specific print method.
#' @export
print.xspliner <- function(x, predictor, ...) {
  if (missing(predictor)) {
    class(x) <- "glm"
    return(print(x, ...))
  }
  if (predictor %in% specials(x, "quantitative")) {
    return(mgcv::print.gam(transition(x, predictor, "base"), ...))
  }
  if (predictor %in% specials(x, "qualitative")) {
    return(print(transition(x, predictor, "base"), ...))
  }
  if (!(predictor %in% specials(x, "qualitative"))) {
    message("Variable was not transformed.")
    class(x) <- "glm"
    return(print(x, ...))
  }
}
