get_spline_formula <- function(response_var, pred_var, env, ...) {
  formula_call <- substitute(list(pred_var, ...))
  formula_call[[1]] <- quote(s)
  formula_call[[2]] <- quote(predictor)
  formula_call <- sub("predictor", pred_var, deparse(formula_call), fixed = TRUE)
  formula <- as.formula(sprintf("%s ~ %s", response_var, formula_call), env = env)
  formula
}

#' Approximate spline on data
#'
#' It aproximates data with spline function by fitting GAM model.
#' @param bb_response_data Blackbox response data, for example pdp curve.
#' @param response_var Name of response value from bb_response_data.
#' @param pred_var Name of predictor value from bb_response_data.
#' @param env Formula environment that should be used for fitting gam model.
#' @param ... Other arguments passed to \link{mgcv::s} function.
#' @return
#' Object of class "gam". See \link{mgcv::gamObject}
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' env <- new.env()
#' approx_with_spline(data.frame(x = x, y = y), "y", "x", env)
approx_with_spline <- function(bb_response_data, response_var, pred_var, env = parent.frame(), ...) {
  s <- mgcv::s
  formula <- get_spline_formula(response_var, pred_var, env, ...)
  mgcv::gam(formula, data = bb_response_data)
}

#' Approximate monotonic spline on data
#'
#' It aproximates data with monotonic spline function by fitting GAM model.
#' @param bb_response_data. Blackbox response data, for example pdp curve.
#' @param response_var Name of response value from bb_response_data.
#' @param pred_var Name of predictor value from bb_response_data.
#' @param env Formula environment that should be used for fitting gam model.
#' @param increasing If TRUE, spline approximation is increasing, if FALSE decreasing.
#' @param ... Other arguments passed to \link{mgcv::s} function.
#' @return
#' Object of class "gam". See \link{mgcv::gamObject}
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' env <- new.env()
#' approx_with_monotonic_spline(data.frame(x = x, y = y), "y", "x", env, TRUE)
approx_with_monotonic_spline <- function(bb_response_data, response_var,
                                         pred_var, env = parent.frame(), increasing, ...) {
  s <- mgcv::s
  formula <- get_spline_formula(response_var, pred_var, env, ...)
  G <- mgcv::gam(formula, data = bb_response_data, fit = FALSE)
  contraint_sign <- if (increasing) 1 else -1
  gam_init <- mgcv::gam(G = G)

  ## generate constraints, by finite differencing
  ## using predict.gam ....
  eps <- 1e-7
  x_range <- range(bb_response_data[[pred_var]])
  diff_grid_0 <- diff_grid_1 <- data.frame(x = seq(x_range[1], x_range[2], length.out = 100))
  colnames(diff_grid_0) <- colnames(diff_grid_1) <- pred_var
  diff_grid_1$x <- diff_grid_1[[pred_var]] + eps
  spline_vals_on_interv_start <- predict(gam_init, newdata = diff_grid_0, type = "lpmatrix")
  spline_vals_on_interv_end <- predict(gam_init, newdata = diff_grid_1, type = "lpmatrix")
  x_var_constraints <- contraint_sign * (spline_vals_on_interv_end - spline_vals_on_interv_start) / eps ## Xx %*% coef(b) must be positive
  G$Ain <- x_var_constraints # inequality constraint matrix
  G$bin <- rep(0, nrow(G$Ain)) # rhs vecctor for constraints
  G$C = matrix(0, 0, ncol(G$X)) # equality constraints (0 means lack of contraint)
  G$sp <- gam_init$sp # smoothing parameter array
  G$p <- coef(gam_init) # initial coeficients array
  G$off <- G$off - 1 # to match what pcls is expecting (moving index of penalty matrix)
  ## force inital parameters to meet constraint
  G$p[-1] <- 0
  coeffs <- mgcv::pcls(G) ## constrained fit
  gam_init$coefficients <- coeffs
  gam_init
}

prepare_pdp_params <- function() {

}

prepare_spline_params_pdp <- function(formula_details, component_details, blackbox, data) {
  method_params <- component_details$method_opts
  method_params[["type"]] <- NULL
  method_params[["object"]] <- blackbox
  method_params[["pred.var"]] <- component_details$var
  method_params[["train"]] <- data

  blackbox_response_obj <- do.call(pdp::partial, method_params)

  spline_params <- component_details$transform_opts
  spline_params[["bb_response_data"]] <- blackbox_response_obj # attr(blackbox_response_obj, "partial.data") do.call loses attributes
  spline_params[["pred_var"]] <- component_details$var
  spline_params[["response_var"]] <- "yhat"
  spline_params[["env"]] <- attr(formula_details$formula, ".Environment")

  spline_params
}

prepare_spline_params_ale <- function(formula_details, component_details, blackbox, data) {
  method_params <- component_details$method_opts
  method_params[["type"]] <- NULL
  method_params[["X.model"]] <- blackbox
  method_params[["J"]] <- component_details$var
  method_params[["X"]] <- data
  method_params[["pred.fun"]] <- function(X.model, newdata) predict(object = X.model, newdata = newdata)
  method_params[["NA.plot"]] <- FALSE

  plot_container <- tempfile()
  pdf(plot_container)
  blackbox_response_obj <- do.call(ALEPlot::ALEPlot, method_params)
  dev.off()
  unlink(plot_container)

  blackbox_response_obj <- data.frame(blackbox_response_obj$x.values, blackbox_response_obj$f.values)
  names(blackbox_response_obj) <- c(component_details$var, "yhat")

  spline_params <- component_details$transform_opts
  spline_params[["bb_response_data"]] <- blackbox_response_obj
  spline_params[["pred_var"]] <- component_details$var
  spline_params[["response_var"]] <- "yhat"
  spline_params[["env"]] <- attr(formula_details$formula, ".Environment")

  spline_params
}

prepare_transform_params_fM <- function(formula_details, component_details, blackbox, data) {
  method_params <- component_details$method_opts
  method_params[["type"]] <- NULL
  method_params[["object"]] <- blackbox
  method_params[["pred.var"]] <- component_details$var
  method_params[["train"]] <- data
  method_params[["ice"]] <- TRUE

  blackbox_response_obj <- do.call(pdp::partial, method_params)

  transform_params <- component_details$method_opts
  transform_params[["response"]] <- blackbox_response_obj[, "yhat"]
  transform_params[["factor"]] <- blackbox_response_obj[, component_details$var]
  transform_params[["factorMerger"]] <- blackbox_response_obj

  transform_params
}

factor_component_env <- function(formula_details, component_details, blackbox, data) {
  if (is.null(component_details$method_opts$type)) {
    stop("No specified type for method!")
  }

  transform_params <- switch(component_details$method_opts$type,
    ice = prepare_transform_params_fM(formula_details, component_details, blackbox, data)
  )

  partition_params <- transform_params[c("stat", "value")] %>%
    purrr::keep(~ !is.null(.))
  transform_params[c("stat", "value")] <- NULL
  transform_params$abbreviate <-  FALSE

  blackbox_response_obj <- do.call(factorMerger::mergeFactors, transform_params)

  partition_params$factorMerger <- blackbox_response_obj
  blackbox_response_transform <- do.call(factorMerger::getOptimalPartitionDf, partition_params)

  list(
    blackbox_response_obj = blackbox_response_obj,
    blackbox_response_transform = blackbox_response_transform
  )

}

numeric_component_env <- function(formula_details, component_details, blackbox, data) {
  if (is.null(component_details$method_opts$type)) {
    stop("No specified type for method!")
  }

  spline_params <- switch(component_details$method_opts$type,
    pdp = prepare_spline_params_pdp(formula_details, component_details, blackbox, data),
    ale = prepare_spline_params_ale(formula_details, component_details, blackbox, data)
  )

  if (is.null(spline_params[["increasing"]])) {
    blackbox_response_approx <- do.call(approx_with_spline, spline_params)
  } else {
    blackbox_response_approx <- do.call(approx_with_monotonic_spline, spline_params)
  }

  list(
    blackbox_response_obj = spline_params[["bb_response_data"]],
    blackbox_response_approx = blackbox_response_approx
  )

}

get_common_components_env <- function(formula_details, special_components_details, blackbox, data) {

  xs_env <- list()
  xf_env <- list()

  xs_vars <- formula_details$xs_variables
  xf_vars <- formula_details$xf_variables

  if (length(xs_vars)) {
    xs_env <- special_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% xs_vars) %>%
      purrr::map(function(component_details) numeric_component_env(formula_details, component_details, blackbox, data)) %>%
      purrr::set_names(xs_vars)
  }

  if (length(xf_vars)) {
    xf_env <- special_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% xf_vars) %>%
      purrr::map(function(component_details) factor_component_env(formula_details, component_details, blackbox, data)) %>%
      purrr::set_names(xf_vars)
  }

  list(
    xs_env = xs_env,
    xf_env = xf_env
  )
}

#" Extract fitted spline function.
#'
#' It extracts fitted spline function by \link{xspline_approx_gam}.
#' @param fitted_gam Fitted gam model approximating data.
#' @param ... Other arguments passed to \link{mgcv::predict.gam} method.
#' @return
#' Function of one variable.
#' @examples
#' set.seed(123)
#' x <- rnorm(20, 5, 5)
#' z <- rnorm(20, 0, 10)
#' y <- - sin(x) + z ^ 3 + rnorm(20, 0, 0.1)
#' data <- data.frame(x, y, z)
#' blackbox <- randomForest::randomForest(y ~ ., data)
#'
#' z_var_response <- pdp::partial(blackbox, "z")
#' z_var_response_approx <- approx_with_spline(z_var_response, "yhat", "z")
#' z_env <- list(
#'   blackbox_response_obj = z_var_response,
#'   blackbox_response_approx = z_var_response_approx
#' )
#' z_var_spline <- get_xs_call(z_env, "z")
#' z_range <- attr(z_var_spline, "variable_range")
#' z_axis <- seq(z_range[1], z_range[2], length.out = 50)
#'
#' plot(z, y)
#' lines(z_var_response)
#' lines(z_axis, z_var_spline(z_axis))
#'
#' x_var_response <- pdp::partial(blackbox, "x")
#' x_var_response_approx <- approx_with_spline(x_var_response, "yhat", "x")
#' x_env <- list(
#'   blackbox_response_obj = x_var_response,
#'   blackbox_response_approx = x_var_response_approx
#' )
#' x_var_spline <- get_xs_call(x_env, "x")
#' x_range <- attr(x_var_spline, "variable_range")
#' x_axis <- seq(x_range[1], x_range[2], length.out = 50)
#'
#' plot(x, y)
#' lines(x_var_response)
#' lines(x_axis, x_var_spline(x_axis))
get_xs_call <- function(xs_env, pred_var_name) {
  xs_approximation <- function(pred_var) {
    data <- data.frame(pred_var)
    names(data) <- pred_var_name
    mgcv::predict.gam(xs_env$blackbox_response_approx, newdata = data)
  }
  attr(xs_approximation, "variable_range") <- range(xs_env$blackbox_response_obj[[pred_var_name]])
  xs_approximation
}

get_xf_call <- function(xf_env, pred_var_name) {
  matched_factors <- xf_env$blackbox_response_transform
  if (length(unique(matched_factors$pred)) < 2) {
    return(function(pred_var) pred_var)
  } else {
    function(pred_var) {
      predictor_values <- data.frame(orig = pred_var)
      transformed_predictor <- dplyr::left_join(predictor_values, matched_factors)
      factor(transformed_predictor, levels = unique(matched_factors$pred))
    }
  }
}

is_lm_better_than_approx <- function(data, response, predictor, approx_fun, compare_stat) {
  approx_model_formula <- as.formula(sprintf("%s ~ approx_fun(%s)", response, predictor))
  approx_model <- lm(approx_model_formula, data)
  lm_model_formula <- as.formula(sprintf("%s ~ %s", response, predictor))
  lm_model <- lm(lm_model_formula, data)
  comparison <- compare_stat(approx_model) <= compare_stat(lm_model)
  if (isTRUE(attr(compare_stat, "higher-better"))) {
    comparison
  } else {
    !comparison
  }
}

correct_improved_components <- function(alter, compare_stat, xs, xf, special_components_details, data, response) {

  get_component_call <- function(special_component_details) {
    if (special_component_details$call_fun == "xs") {
      xs
    } else {
      xf
    }
  }

  use_untransformed <- function(special_component_details) {
    call_fun <- special_component_details$call_fun
    if (call_fun == "xs") {
      alter_variable <- switch(alter$numeric,
        always = FALSE,
        auto = is_lm_better_than_approx(data, response, special_component_details$var,
                                        get_component_call(special_component_details), compare_stat),
        never = TRUE
      )
    } else if (call_fun == "xf") {
      alter_variable <- switch(alter$factor,
        always = FALSE,
        never = TRUE
      )
    } else {
      alter_variable <- TRUE
    }

    if (is.null(alter_variable)) {
      alter_variable <- TRUE
    }

    alter_variable
  }

  use_bare_call <- function(special_component_details) {
    special_component_details$new_call <- special_component_details$var
    special_component_details
  }

  special_components_details %>%
    purrr::map_if(use_untransformed, use_bare_call)

}
