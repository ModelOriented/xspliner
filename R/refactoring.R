library(magrittr)

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

extract_call <- function(var_raw, var_call) { # it must be vectorized!!
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

build_spline_formula <- function(formula) {

  formula_variables <- all.vars(formula)[-1]
  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))
  vars <- as.character(attr(formula_terms,"variables"))[-c(1, 2)]

    details <- extract_call(formula_variables, vars) # after extract_call is vectorized
  #iterujemy po vars_list
    rhs_formula <- sub(details$call, details$new_call, rhs_formula, fixed = TRUE) #use purrr::reduce2()??

    attr(rhs_formula, "transform_params") <- details

    rhs_formula
}

xs_call <- list("x" = function(x) x ^ 2)
xf_call <- list("t" = function(t) sqrt(t))
enn <- new.env()
enn$xs_call <- xs_call
enn$xf_call <- xf_call
form_final <- as.formula(sprintf("%s ~ %s", response, rhs_formula), env = enn)

df <- data.frame(y = 1:10, x = 101:110, z = 11:20, t = (0:9)^2, w = 10:1)
model.frame(form_final, data = df)
