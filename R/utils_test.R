data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), w = rexp(10))
oko <- 10
formula <- log(y) ~ xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp", object = oko)) * z + xf(t) + w ^ 2
formula_details <- get_formula_details(formula, data)
additive_components_details <- get_all_components_info(formula_details)
transform_formula_chr(formula_details, additive_components_details)
