build_approximation_formula <- function(response, predictor, env, ...) {
  formula_call <- substitute(list(predictor, ...))
  formula_call[[1]] <- quote(s)
  formula_call[[2]] <- quote(predictor)
  formula_call <- sub("predictor", predictor, deparse(formula_call), fixed = TRUE)
  formula <- as.formula(sprintf("%s ~ %s", response, formula_call), env = env)
  formula
}

#' Approximate spline on data
#'
#' It approximates data with spline function by fitting GAM model.
#' @param effect_data Black box response data, for example pdp curve.
#' @param response Name of response value from effect_data.
#' @param predictor Name of predictor value from effect_data.
#' @param env Formula environment that should be used for fitting gam model.
#' @param monotonic Possible options "up", "down" and "auto. If up the spline is increasing, when down decreasing.
#' @param ... Other arguments passed to \link[mgcv]{s} function.
#' @return
#' Object of class "gam". See \link[mgcv]{gamObject}
#' @examples
#' x <- sort(rnorm(20, 5, 5))
#' y <- rnorm(20, 2, 2)
#' env <- new.env()
#' approx_with_spline(data.frame(x = x, y = y), "y", "x", env)
#'
#' approx_with_monotonic_spline(data.frame(x = x, y = y), "y", "x", env, "up")
#' @export
approx_with_spline <- function(effect_data, response, predictor, env = parent.frame(), ...) {
    log_msg(cat(sprintf("Estimating %s variable..  \n", predictor)))
  s <- mgcv::s
  formula <- build_approximation_formula(response, predictor, env, ...)
  mgcv::gam(formula, data = effect_data)
}

#' @rdname approx_with_spline
#' @export
approx_with_monotonic_spline <- function(effect_data, response,
                                         predictor, env = parent.frame(), monotonic, ...) {
  if (monotonic == "auto") {
    model_up <- approx_with_monotonic_spline(effect_data, response, predictor, env = parent.frame(), "up", ...)
    model_down <- approx_with_monotonic_spline(effect_data, response, predictor, env = parent.frame(), "down", ...)
    if (summary(model_up)$r.sq > summary(model_up)$r.sq) {
      return(model_up)
    } else {
      return(model_down)
    }
  }

  s <- mgcv::s
  formula <- build_approximation_formula(response, predictor, env, ...)
  G <- mgcv::gam(formula, data = effect_data, fit = FALSE)
  contraint_sign <- if (monotonic == "up") 1 else -1
  gam_init <- mgcv::gam(G = G)

  ## generate constraints, by finite differencing
  ## using predict.gam ....
  eps <- 1e-7
  x_range <- range(effect_data[[predictor]])
  diff_grid_0 <- diff_grid_1 <- data.frame(x = seq(x_range[1], x_range[2], length.out = 100))
  colnames(diff_grid_0) <- colnames(diff_grid_1) <- predictor
  diff_grid_1$x <- diff_grid_1[[predictor]] + eps
  spline_vals_on_interv_start <- mgcv::predict.gam(gam_init, newdata = diff_grid_0, type = "lpmatrix")
  spline_vals_on_interv_end <- mgcv::predict.gam(gam_init, newdata = diff_grid_1, type = "lpmatrix")
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

prepare_transition_params_pdp <- function(formula_metadata, component_details, blackbox, data) {
  effect <- component_details$effect
  effect[["type"]] <- NULL
  effect[["object"]] <- blackbox
  effect[["pred.var"]] <- component_details$var
  if (is.null(effect[["train"]])) {
    effect[["train"]] <- data
  }
  effect[["which.class"]] <- 2 # for glm 1st level is failure

  transition <- component_details$transition
  if (transition[["alter"]] == "never") {
    transition[["effect_data"]] <- NULL
    return(transition)
  }

  effect_outcome <- do.call(pdp::partial, effect)
  attr(effect_outcome, "type") <- "pdp"

  transition[["effect_data"]] <- effect_outcome
  transition[["predictor"]] <- component_details$var
  transition[["response"]] <- "yhat"
  transition[["env"]] <- attr(formula_metadata$formula, ".Environment")

  transition
}

prepare_transition_params_ale <- function(formula_metadata, component_details, blackbox, data) {
  effect <- component_details$effect
  effect[["type"]] <- NULL
  effect[["X.model"]] <- blackbox
  effect[["J"]] <- component_details$var
  effect[["X"]] <- data
  effect[["pred.fun"]] <- function(X.model, newdata) predict(object = X.model, newdata = newdata)
  effect[["NA.plot"]] <- FALSE

  plot_container <- tempfile()
  grDevices::pdf(plot_container)
  effect_outcome <- do.call(ALEPlot::ALEPlot, effect)
  grDevices::dev.off()
  unlink(plot_container)

  effect_outcome <- data.frame(effect_outcome$x.values, effect_outcome$f.values)
  attr(effect_outcome, "type") <- "ale"
  names(effect_outcome) <- c(component_details$var, "yhat")

  transition <- component_details$transition
  transition[["effect_data"]] <- effect_outcome
  transition[["predictor"]] <- component_details$var
  transition[["response"]] <- "yhat"
  transition[["env"]] <- attr(formula_metadata$formula, ".Environment")

  transition
}

prepare_transition_params_ice <- function(formula_metadata, component_details, blackbox, data) {
  effect <- component_details$effect
  effect[["type"]] <- NULL
  effect[["object"]] <- blackbox
  effect[["pred.var"]] <- component_details$var
  if (is.null(effect[["train"]])) {
    effect[["train"]] <- data
  }
  effect[["ice"]] <- TRUE

  transition <- component_details$transition

  if (transition[["alter"]] == "never") {
    transition[["effect_data"]] <- NULL
    return(transition)
  }

  effect_outcome <- do.call(pdp::partial, effect)
  attr(effect_outcome, "type") <- "ice"
  transition[["response"]] <- effect_outcome[["yhat"]]
  transition[["factor"]] <- effect_outcome[[component_details$var]]
  transition[["effect_data"]] <- effect_outcome

  transition
}

get_qualitative_transition <- function(formula_metadata, component_details, blackbox, data) {
  if (is.null(component_details$effect$type)) {
    stop("No specified type for method!")
  }
  transition <- switch(component_details$effect$type,
    ice = prepare_transition_params_ice(formula_metadata, component_details, blackbox, data)
  )
  alter <- transition[["alter"]]
  transition[["alter"]] <- NULL

  if (alter == "never") {
    quantitative_transition <- list(
      effect_outcome = NULL,
      transition_outcome = NULL
    )
  } else {
    partition_params <- transition[c("stat", "value")] %>%
      purrr::keep(~ !is.null(.))
    transition[c("stat", "value")] <- NULL
    transition$abbreviate <-  FALSE

    transition_outcome <- do.call(factorMerger::mergeFactors, transition)

    partition_params$factorMerger <- transition_outcome
    partition <- do.call(factorMerger::getOptimalPartitionDf, partition_params)
    attr(transition_outcome, "partition") <- partition

    quantitative_transition <- list(
      effect_outcome = transition[["effect_data"]],
      transition_outcome = transition_outcome
    )
  }

  quantitative_transition
}

get_quantitative_transition <- function(formula_metadata, component_details, blackbox, data,
                                        family, compare_stat) {
  if (is.null(component_details$effect$type)) {
    stop("No specified type for method!")
  }

  transition <- switch(component_details$effect$type,
    pdp = prepare_transition_params_pdp(formula_metadata, component_details, blackbox, data),
    ale = prepare_transition_params_ale(formula_metadata, component_details, blackbox, data)
  )

  monotonic <- transition[["monotonic"]]
  alter <- transition[["alter"]]
  transition[["alter"]] <- NULL

  if (alter == "never") {
    quantitative_transition <- list(
      effect_outcome = NULL,
      transition_outcome = NULL
    )
  } else {
    if (monotonic == "not") {
      transition[["monotonic"]] <- NULL
      transition_outcome <- do.call(approx_with_spline, transition)
    } else {
      transition_outcome <- do.call(approx_with_monotonic_spline, transition)
    }

    quantitative_transition <- list(
      effect_outcome = transition[["effect_data"]],
      transition_outcome = transition_outcome
    )
  }

  if (alter == "auto") {
    xs_function <- build_xs_function(quantitative_transition, component_details$var)
    lm_better <- is_lm_better_than_xs(
      data, formula_metadata$lhs, component_details$var, xs_function, family, compare_stat)
    if (lm_better) {
      message(sprintf("%s fits better than %s. Using bare component.",
                      component_details$var, component_details$new_call))
      quantitative_transition <- list(
        effect_outcome = NULL,
        transition_outcome = NULL
      )
    }
  }

  quantitative_transition
}

is_lm_better_than_xs <- function(data, lhs, var, xs_function, family, compare_stat) {
  xs_function <- xs_function
  linear_formula <- as.formula(sprintf("%s ~ %s", lhs, var))
  transition_formula <- as.formula(sprintf("%s ~ xs_function(%s)", lhs, var))
  linear_model <- glm(linear_formula, data = data, family = family)
  transition_model <- glm(transition_formula, data = data, family = family)
  comparison <- compare_stat(transition_model) <= compare_stat(linear_model)
  if (isTRUE(attr(compare_stat, "higher-better"))) {
    lm_better <- comparison
  } else {
    lm_better <- !comparison
  }
}

get_transitions_outcome <- function(formula_metadata, special_components_details,
                                    blackbox, data, family, compare_stat) {

  quantitative_variables <- formula_metadata$xs_variables
  qualitative_variables <- formula_metadata$xf_variables

  quantitative <- if (length(quantitative_variables)) {
    special_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% quantitative_variables) %>%
      purrr::map(function(component_details) get_quantitative_transition(
        formula_metadata, component_details, blackbox, data, family, compare_stat)) %>%
      purrr::set_names(quantitative_variables)
  } else {
    list()
  }

  qualitative <- if (length(qualitative_variables)) {
    special_components_details %>%
      purrr::keep(function(component_details) component_details[["var"]] %in% qualitative_variables) %>%
      purrr::map(function(component_details) get_qualitative_transition(
        formula_metadata, component_details, blackbox, data)) %>%
      purrr::set_names(qualitative_variables)
  } else {
    list()
  }

  list(
    quantitative = quantitative,
    qualitative = qualitative
  )
}

build_xs_function <- function(quantitative_transition, predictor_name) {
  xs_approximation <- function(predictor) {
    data <- data.frame(predictor)
    names(data) <- predictor_name
    mgcv::predict.gam(quantitative_transition$transition_outcome, newdata = data)
  }
  attr(xs_approximation, "variable_range") <- range(quantitative_transition$effect_outcome[[predictor_name]])
  xs_approximation
}

build_xf_function <- function(qualitative_transition, predictor_name) {
  matched_factors <- attr(qualitative_transition$transition_outcome, "partition")
  if (length(unique(matched_factors$pred)) < 2) {
    return(function(predictor) predictor)
  } else {
    function(predictor) {
      predictor_values <- data.frame(orig = predictor)
      suppressMessages(transformed_predictor <- dplyr::left_join(predictor_values, matched_factors, by = "orig"))
      factor(transformed_predictor$pred, levels = unique(matched_factors$pred))
    }
  }
}
