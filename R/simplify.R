library(magrittr)

formula <- log(y) ~ xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp", bb = rf_model)) * z + xf(t) + w ^ 2

get_formula_details <- function(formula) {
  formula_variables <- all.vars(formula)
  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))

  list(
    formula = formula,
    formula_variables = formula_variables,
    response = formula_variables[1],
    pred_vars = formula_variables[-1],
    formula_labels = attr(formula_terms, "term.labels"),
    formula_xs_position = attr(formula_terms, "specials")$xs - 1,
    formula_xf_position = attr(formula_terms, "specials")$xf - 1,
    xs_variables = formula_variables[attr(formula_terms, "specials")$xs],
    xf_variables = formula_variables[attr(formula_terms, "specials")$xf]
  )
}


# prepare_call_full <- function(string) {
#   fun <- substr(string, 1, 2)
#   if (!(fun %in% c("xs", "xf"))) {
#     return(string)
#   }
#
#   var <- sub("(,|\\)).*$", '', string) %>%
#     substr(4, nchar(.))
#
#   new_call <- sprintf("%s_call('%s')(%s)", fun, var, var)
#   list(
#
#   )
#
# }

# build_spline_formula <- function(formula_details, env) {
#   formula <- formula_details$formula
#   tt <- terms(formula, specials = c("xs", "xf"))
#   labels <- attr(tt, "term.labels")
#   new_formula_terms <- prepare_call(labels)
#   as.formula(reformulate(new_formula_terms), env = env)
# }

## tests
# formula_details <- get_formula_details(formula)
# xs_call <- list("x" = function(x) x ^ 3)
# xf_call <- list()
# oko <- build_spline_formula(formula_details, c(xs_call, xf_call))
##

## another approach
formula <- log(y) ~ xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp")) * z + xf(t) + w ^ 2
response <- as.character(formula)[[2]]
rhs_formula <- as.character(formula)[[3]]
#----

prepare_call <- function(string) {
  fun <- substr(string, 1, 2)
  if (!(fun %in% c("xs", "xf"))) {
    return(string)
  }

  var <- sub("(,|\\)).*$", '', string) %>%
    substr(4, nchar(.))

  sprintf("%s_call[['%s']](%s)", fun, var, var)
}

extract_call <- function(var_raw, var_call) {
  if (var_raw == var_call) {
    return(
      list(
        var = var_raw,
        call = var_call,
        new_call = var_raw,
        spline_opts = NULL,
        method_opts = NULL
      )
    )
  }

  new_call <- prepare_call(var_call)
  spline_params <- as.list(parse(text = var_call[1])[[1]])
  spline_opts <- eval(spline_params$spline_opts)
  method_opts <- eval(spline_params$method_opts)

  list(
    var = var_raw,
    call = var_call,
    new_call = new_call,
    spline_opts = spline_opts,
    method_opts = method_opts
  )
}

formula_variables <- all.vars(formula)[-1]
formula_terms <- terms.formula(formula, specials = c("xs", "xf"))
vars <- as.character(attr(formula_terms,"variables"))[-c(1, 2)]

#iterujemy po vars_list
for (i in seq_along(formula_variables)) {
  details <- extract_call(formula_variables[i], vars[i])
  rhs_formula <- sub(details$call, details$new_call, rhs_formula, fixed = TRUE)
}
rhs_formula

xs_call <- list("x" = function(x) x ^ 2)
xf_call <- list("t" = function(t) sqrt(t))
enn <- new.env()
enn$xs_call <- xs_call
enn$xf_call <- xf_call
form_final <- as.formula(sprintf("%s ~ %s", response, rhs_formula), env = enn)

df <- data.frame(y = 1:10, x = 101:110, z = 11:20, t = (0:9)^2, w = 10:1)
model.frame(form_final, data = df)


###
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



