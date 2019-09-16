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

plot_quantitative <- function(x, variable_name, plot_response, plot_approx, data, plot_data, plot_deriv, use_coeff) {
  if (plot_data && is.null(data)) {
    message("You can plot data points only when data parameter is provided.")
    plot_data <- FALSE
  }

  to_plot <- c(plot_data, plot_response, plot_approx, plot_deriv)
  if (!any(to_plot)) {
    stop("You must specify at least one plot.")
  }
  transition_fun <- transition(x, variable_name, "function")
  variable_coeff <- 1
  if (use_coeff) {
    variable_coeff <- setNames(x$coefficients[-1], all.vars(x$call$formula)[-1])[variable_name]
  }

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
    data$yhat <- variable_coeff * data$yhat
    colnames(data)[colnames(data) == "yhat"] <- response_var
    names(color_values)[2] <- attr(data, "type")
    data$type <- attr(data, "type")
    base_data <- rbind(base_data, data)
  }
  if (plot_approx) {
    x_var <- seq(from = plot_range[1], to = plot_range[2], length.out = 50)
    y_var <- variable_coeff * transition_fun(x_var)
    data <- data.frame(y_var, x_var)
    colnames(data) <- c(response_var, variable_name)
    data$type <- "approximation"
    base_data <- rbind(base_data, data)
  }
  if (plot_deriv) {
    eps <- (plot_range[2] - plot_range[1]) / 500
    x_var <- seq(from = plot_range[1], to = plot_range[2], length.out = 50)[-50]
    y_var <- variable_coeff * (transition_fun(x_var + eps) - transition_fun(x_var)) / eps
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

#' Plot variable profile
#'
#' @description The function plots variable profile.
#' In case of quantiitative variable it plots original transition function and its spline approximation.
#' The function provides possibility to plot data points and transition derivative as well.
#' In case of qualitative variable it plots merging path for variable levels.
#' When no variable is specified it plots transitions for first \code{n_plots} variables.
#'
#' @param variable_names Names of predictors which transitions should be plotted.
#' @param plot_response If TRUE black box model response is drawn.
#' @param plot_approx If TRUE black box model response approximation is drawn.
#' @param data Training data used for building \code{x} model. Required for plot_data option.
#' @param plot_data If TRUE raw data is drawn.
#' @param plot_deriv If TRUE derivative of approximation is showed on plot.
#' @param n_plots Threshold for number of plots when plotting all variables.
#' @param use_coeff If TRUE both PDP function and its approximation is scaled with corresponding surrogate model coefficient.
#'
#' @examples
#' library(randomForest)
#' set.seed(1)
#' data <- iris
#' # regression model
#' iris.rf <- randomForest(Petal.Width ~  Sepal.Length + Petal.Length + Species, data = data)
#' iris.xs <- xspline(iris.rf)
#' # plot Sepal.Length transition
#' plot_variable_transition(iris.xs, "Sepal.Length")
#' # plot Species transition
#' plot_variable_transition(iris.xs, "Species")
#' # plot all transitions
#' plot_variable_transition(iris.xs)
#' # plot Sepal.Length transition, its derivative and data points
#' plot_variable_transition(iris.xs, "Sepal.Length", data = data, plot_data = TRUE, plot_deriv = TRUE)
#'
#' @export
plot_variable_transition <- function(x, variable_names = NULL, plot_response = TRUE, plot_approx = TRUE,
                                     data = NULL, plot_data = FALSE, plot_deriv = FALSE, n_plots = 6, use_coeff = TRUE) {
  if (is.null(variable_names)) {
    special_vars <- specials(x, "all")
    special_vars_to_plot <- special_vars[1:min(n_plots, length(special_vars))]
    plot_specials_grid(x, special_vars_to_plot, plot_response, plot_approx, data, plot_data, plot_deriv, use_coeff)
  } else if (length(variable_names) > 1) {
    special_vars <- specials(x, "all")
    special_vars_to_plot <- intersect(special_vars, variable_names)
    if (length(special_vars_to_plot) == 0) {
      stop("None of selected variables was transformed.")
    }
    plot_specials_grid(x, special_vars_to_plot, plot_response, plot_approx, data, plot_data, plot_deriv, use_coeff)
  } else if (!(variable_names %in% specials(x, "all"))) {
    stop("Variable wasn't transformed.")
  } else if (variable_names %in% specials(x, "qualitative")) {
    plot(transition(x, variable_names, "base"))
  } else {
    plot_quantitative(x, variable_names, plot_response, plot_approx, data, plot_data, plot_deriv, use_coeff)
  }
}

#' Plot models comparison
#'
#' @description The function plots models comparison based on them predictions.
#'
#' @param x Object of class 'xspliner'
#' @param model Base model that xspliner is based on.
#' @param data Dataset on which predictions should be compared.
#' @param compare_with Named list. Other models that should be compared with xspliner and \code{model}.
#' @param sort_by When comparing models determines according to which model should observations be ordered.
#' @param prediction_funs Prediction functions that should be used in model comparison.
#'
#' @examples
#' iris_data <- droplevels(iris[iris$Species != "setosa", ])
#' library(e1071)
#' library(randomForest)
#' library(xspliner)
#' # Build SVM model, random forest model and surrogate one constructed on top od SVM
#' model_svm <- svm(Species ~  Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'                  data = iris_data, probability = TRUE)
#' model_rf <- randomForest(Species ~  Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris_data)
#'
#' model_xs <- xspline(Species ~  xs(Sepal.Length) + xs(Sepal.Width) + xs(Petal.Length) + xs(Petal.Width),
#'                     model = model_svm)
#' # Prepare prediction functions returning label probability
#' prob_svm <- function(object, newdata) attr(predict(object, newdata = newdata, probability = TRUE), "probabilities")[, 2]
#' prob_rf <- function(object, newdata) predict(object, newdata = newdata, type = "prob")[, 2]
#' prob_xs <- function(object, newdata) predict(object, newdata = newdata, type = "response")
#'
#' # Plotting predictions for original SVM and surrogate model on training data
#' plot_model_comparison(
#'   model_xs, model_svm, data = iris_data,
#'   prediction_funs = list(xs = prob_xs, svm = prob_svm)
#' )
#' # Plotting predictions for original SVM, surrogate model and random forest on training data
#' plot_model_comparison(
#'   model_xs, model_svm, data = iris_data,
#'   compare_with = list(rf = model_rf),
#'   prediction_funs = list(xs = prob_xs, svm = prob_svm, rf = prob_rf)
#' )
#' # Sorting values according to SVM predictions
#' plot_model_comparison(
#'   model_xs, model_svm, data = iris_data,
#'   compare_with = list(rf = model_rf),
#'   prediction_funs = list(xs = prob_xs, svm = prob_svm, rf = prob_rf),
#'   sort_by = "svm"
#' )
#'
#' @export
plot_model_comparison <- function(x, model, data, compare_with = list(),
                                  prediction_funs = list(function(object, newdata) predict(object, newdata)), sort_by = NULL) {
  if (missing(data)) {
    stop("Data must be provided.")
  }
  model_name <- rev(as.character(model$call[[1]]))[1]
  models_list <- list(xspliner = x)
  models_list[[model_name]] <- model
  models_list <- append(models_list, compare_with)
  if (length(prediction_funs) == 1) {
    fitted <- models_list %>%
      purrr::map(~ prediction_funs[[1]](., data))
  } else {
    if (length(models_list) != length(prediction_funs)) {
      stop("prediction_funs should provide prediction functions for all models (surrogate, original and model to compare), or common one.")
    }
    fitted <- models_list %>%
      purrr::map2(prediction_funs, function(model, pred_fun) pred_fun(model, data))
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

plot_specials_grid <- function(x, vars, plot_response, plot_approx, data, plot_data, plot_deriv, use_coeff) {
  plot_list <- list(nrow = ceiling(length(vars) / 3))
  plot_list[["ncol"]] <- ceiling(length(vars) / plot_list$nrow)
  for (var in vars)  {
    plot_list[[var]] <- plot_variable_transition(x, var, plot_response, plot_approx, data, plot_data, plot_deriv, use_coeff = use_coeff)
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
