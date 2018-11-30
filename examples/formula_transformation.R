type <- "pdp"
formula <- log(y) ~
  xs(x, effect = list(type = type)) * log(z) + xf(t) + w ^ 2 + log(a) + sqrt(exp(b)) + xs(d)
data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10),
                   w = rexp(10), a = rnorm(10), b = rnorm(10), d = rnorm(10))
variable_names <- extract_formula_var_names(formula, data)
formula_metadata <- get_formula_metadata(formula, variable_names)
special_components_details <- collect_specials_metadata(formula_metadata)
transform_formula_chr(formula_metadata, special_components_details)

