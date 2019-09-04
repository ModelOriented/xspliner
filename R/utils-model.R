specials <- function(model, type = "all") {
  predictors_quant <- names(environment(model)$quantitative_transitions)
  predictors_qual <- names(environment(model)$qualitative_transitions)
  if (type == "quantitative") {
    return(predictors_quant)
  } else if (type == "qualitative") {
    return(predictors_qual)
  } else {
    c(predictors_quant, predictors_qual)
  }
}

plot_quantitative <- function(x, variable_name, plot_response, plot_approx, data, plot_data, plot_deriv) {
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
    plot_range <- range(data[[variable_name]])
  } else {
    plot_range <- attr(transition_fun, "variable_range")
  }

  response_var <- environment(x)$response
  base_data <- data.frame(x = numeric(), y = numeric(), type = character())
  colnames(base_data)[1:2] <- c(variable_name, response_var)
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
      return(base_plot)
    } else {
      scaled <- data_linear_scaling(base_data[[response_var]], data[[response_var]])
      data[[response_var]] <- scaled$data
      data$type <- "derivative"
      base_data <- rbind(base_data, data)
    }

  }

  base_plot <- ggplot2::ggplot(data = base_data,
                               ggplot2::aes_string(x = variable_name, y = response_var, colour = "type")) +
    ggplot2::geom_point(data = base_data[base_data$type == "data", ]) +
    ggplot2::geom_line(data = base_data[base_data$type != "data", ])

  if (plot_deriv) {
    base_plot + ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(~ scaled$scaling * . + scaled$translation,
                                   name = "Transformation derivative")) +
      ggplot2::scale_color_manual(
        values = color_values) +
      ggplot2::labs(colour = "Plot type") +
      ggplot2::theme_minimal()
  } else {
    base_plot +
      ggplot2::scale_color_manual(
        values = color_values) +
      ggplot2::labs(colour = "Plot type") +
      ggplot2::theme_minimal()
  }

}

#' To avoid CRAN check problems
utils::globalVariables(c("Observation", "Model", "Value"))

plot_model_comparison <- function(x, model, data, compare_with, prediction_functions, sort_by = NULL) {
  model_name <- rev(as.character(model$call[[1]]))[1]
  models_list <- list(xspliner = x)
  models_list[[model_name]] <- model
  models_list <- append(models_list, compare_with)
  if (length(prediction_functions) == 1) {
    fitted <- models_list %>%
      purrr::map(~ prediction_functions[[1]](., data))
  } else {
    if (length(models_list) != length(prediction_functions)) {
      stop("prediction_functions should provide prediction functions for all models (surrogate, original and model to compare), or common one.")
    }
    fitted <- models_list %>%
      purrr::map2(prediction_functions, function(model, pred_fun) pred_fun(model, data))
  }

  fitted_values <- as.data.frame(fitted)

  if (!is.null(sort_by)) {
    fitted_values <- fitted_values[order(fitted_values[[sort_by]]), ]
  }

  fitted_values <- fitted_values %>%
    dplyr::mutate(Observation = 1:nrow(.)) %>%
    tidyr::gather(key = "Model", value = "Value", -Observation)

  heatmap_plot <- fitted_values %>%
    ggplot2::ggplot(ggplot2::aes(Observation, Model)) +
    ggplot2::geom_tile(ggplot2::aes(fill = Value)) +
    ggplot2::theme_minimal()

  if (is.character(fitted_values$Value) || is.factor(fitted_values$Value)) {
    colors <- c("#990033", "#0033CC")
    names(colors) <- levels(fitted$xspliner)
    heatmap_plot <- heatmap_plot +
      ggplot2::scale_fill_manual(values = colors)
  }
  heatmap_plot
}

plot_specials_grid <- function(x, vars, plot_response, plot_approx, data, plot_data, plot_deriv) {
  plot_list <- list(nrow = ceiling(length(vars) / 3))
  plot_list[["ncol"]] <- ceiling(length(vars) / plot_list$nrow)
  for (var in vars)  {
    plot_list[[var]] <- plot.xspliner(x, var, NULL, plot_response, plot_approx, data, plot_data, plot_deriv)
  }
  do.call(grid_arrange_shared_legend, plot_list)
}

grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right")) {
  # trick to add one legend: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x)
    x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x)
    x + ggplot2::theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(
    position,
    "bottom" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)
    ),
    "right" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)
    )
  )

  grid::grid.newpage()
  grid::grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

}
