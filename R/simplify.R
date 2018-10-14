get_formula_details <- function(formula) {
  formula_variables <- all.vars(formula)
  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))

  list(
    formula_variables = formula_variables,
    response = formula_variables[1],
    pred_vars = formula_variables[-1],
    formula_labels = attr(formula_terms, "term.labels"),
    formula_xs_position = attr(formula_terms, "specials")$xs - 1,
    formula_xf_position = attr(formula_terms, "specials")$xf - 1,
    xs_variables <- formula_variables[attr(formula_terms, "specials")$xs],
    xf_variables <- formula_variables[attr(formula_terms, "specials")$xf]
  )
}

formula <- y ~ xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp")) + z

xp_gam <- function(formula, blackbox, data = model.frame(blackbox)) {
  xs_call <- list()
  xf_call <- list()

  formula_details <- get_formula_details(formula)

  for (xs_var in formula_details$xs_variables) {
    spline_params <- as.list(parse(text = formula_details$formula_labels[1])[[1]])
    spline_opts <- eval(spline_params$spline_opts)
    method_opts <- eval(spline_params$method_opts)

    bb_response_func <- do.call(get_bb_response, method_opts) # for example pdp curve
    bb_response_approx <- do.call(get_bb_response_approx, spline_opts) # estimation of pdp
    xs_call[[xs_var]] <- bb_response_approx
  }

  # find out how parent.frame works to get below correctly (maybe we don't want it):
  transformed_formula <- build_interp_formula(formula_details, env = parent.frame()) # we want "response ~ xs_call('x1')(x1) + xf_call('x2')(x2)"
  model <- glm(transformed_formula)

  list(
    xs_call = xs_call,
    xf_call = xf_call,
    model = model
  )
}

xp_gam_predict <-

xs <- function() {

}

terms.formula(y ~ xs(x) + xf(z) + t, data)
