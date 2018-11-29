#' Plot method for 'xspliner' model
#'
#' @param x Object of class 'xspliner'
#' @param variable_name Name of predictor that should be plotted.
#' @param plot_response If TRUE blackbox model response is drawn.
#' @param plot_approx If TRUE blackbox model response approcimation is drawn.
#' @param plot_data If TRUE raw data is drawn.
#' @param data Training data used for building \code{x} model. Required for plot_data option.
#'
#' @export
plot.xspliner <- function(x, variable_name = NULL, plot_response = TRUE,
                           plot_approx = TRUE, data = NULL, plot_data = FALSE, ...) {
  # (todo) this needs to be rewritten, the code is really bad
  if (is.null(variable_name)) {
    stats:::plot.lm(x, ...)
  } else {
    if (plot_data && is.null(data)) {
      warning("You can plot data points only when data parameter is provided.")
      plot_data <- FALSE
    }

    plots <- c("data", "response", "approx")[c(plot_data, plot_response, plot_approx)]
    if (length(plots) == 0) {
      stop("You must specify at least one plot.")
    }

    response_var <- environment(x)$response
    xp_call <- environment(x)$xs_call[[variable_name]]

    if (plot_data) {
      plot_range <- range(data[, variable_name])
    } else {
      plot_range <- attr(xp_call, "variable_range")
    }

    plot_data_data <- data

    plot_response_data <- environment(x)$xs_env_list[[variable_name]]$blackbox_response_obj
    colnames(plot_response_data) <- c(variable_name, response_var)

    x_approx <- seq(plot_range[1], plot_range[2], length.out = nrow(plot_response_data))
    y_approx <- xp_call(x_approx)
    plot_approx_data <- data.frame(x_approx, y_approx)
    colnames(plot_approx_data) <- c(variable_name, response_var)

    if (plots[1] == "data") {
      base_plot <- ggplot2::ggplot(data = plot_data_data, ggplot2::aes_string(x = variable_name, y = response_var)) +
        ggplot2::geom_point()
      if (length(plots) == 1) {
        return(base_plot + ggplot2::theme_minimal())
      }
      if (length(plots) == 2) {
        second_data <- switch(
          plots[2],
          response = plot_response_data,
          approx = plot_approx_data
        )
        plot_color <- switch(
          plots[2],
          response = "red",
          approx = "blue"
        )
        return(
          base_plot +
            ggplot2::geom_line(
              data = second_data, ggplot2::aes_string(x = variable_name, y = response_var), color = plot_color) +
            ggplot2::theme_minimal()
        )
      }
      if (length(plots) == 3) {
        return(
          base_plot +
            ggplot2::geom_line(
              data = plot_response_data, ggplot2::aes_string(x = variable_name, y = response_var), color = "red") +
            ggplot2::geom_line(
              data = plot_approx_data, ggplot2::aes_string(x = variable_name, y = response_var), color = "blue") +
            ggplot2::theme_minimal()
        )
      }
    } else if (plots[1] == "response") {
      base_plot <- ggplot2::ggplot(data = plot_response_data, ggplot2::aes_string(x = variable_name, y = response_var)) +
        ggplot2::geom_line(color = "red")
      if (length(plots) == 1) {
        return(base_plot + ggplot2::theme_minimal())
      }
      if (length(plots) == 2) {
        return(
          base_plot +
            ggplot2::geom_line(
              data = plot_approx_data, ggplot2::aes_string(x = variable_name, y = response_var), color = "blue") +
            ggplot2::theme_minimal()
        )
      }
    } else {
      return(
        ggplot2::ggplot(
          data = plot_approx_data, ggplot2::aes_string(x = variable_name, y = response_var), color = "blue") +
          ggplot2::geom_line()
      )
    }
  }
}

