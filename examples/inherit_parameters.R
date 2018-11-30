# build some external variables
data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), w = rexp(10))
oko <- 10

# construct formula
formula <- log(y) ~
  xs(x, effect = list(k = 6), method_opts = list(type = "pdp", object = oko)) * z + xf(t) + w ^ 2

# check if library functions inherits variables from parent.frame()
formula_var_names <- extract_formula_var_names(formula, data)
formula_metadata <- get_formula_metadata(formula, formula_var_names)
additive_components_details <- collect_specials_metadata(formula_metadata)
transform_formula_chr(formula_metadata, additive_components_details)
