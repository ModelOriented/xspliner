type <- "pdp"
formula <- log(y) ~
  xs(x, method_opts = list(type = type)) * log(z) + xf(t) + w ^ 2 + log(a) + sqrt(exp(b)) + xs(d)
data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10),
                   w = rexp(10), a = rnorm(10), b = rnorm(10), d = rnorm(10))
variable_names <- extract_formula_var_names(formula, data)
formula_details <- get_formula_details(formula, variable_names)
special_components_details <- get_special_components_info(formula_details)
transform_formula_chr(formula_details, special_components_details)

