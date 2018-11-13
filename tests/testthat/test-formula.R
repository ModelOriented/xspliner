context("test-formula")

test_that("formula variables are extracted properly", {
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), w = rexp(10))
  is_centered <- FALSE
  formula_1 <- y ~ log(x) + xs(z, spline_opts = list(k = 6))
  formula_2 <- log(y) ~
    xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp", center = is_centered)) * z + xf(t) + w ^ 2

  expect_equal(extract_formula_var_names(formula_1), c("y", "x", "z"))
  expect_equal(extract_formula_var_names(formula_1, data), c("y", "x", "z"))
  expect_equal(extract_formula_var_names(formula_2), c("y", "x", "is_centered", "z", "t", "w")) #22
  expect_equal(extract_formula_var_names(formula_2, data), c("y", "x", "z", "t", "w"))
})

test_that("get_formula_details returns formula info in correct form", {
  is_centered <- FALSE
  formula <- log(y) ~
    xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp", center = is_centered)) * z + xf(t) + w ^ 2
  variable_names <- c("y", "x", "z", "t", "w")
  formula_details <- get_formula_details(formula, variable_names)

  expect_equal(formula_details$formula, formula)
  expect_equal(formula_details$raw_response, "y")
  expect_equal(formula_details$raw_pred_vars, c("x", "z", "t", "w"))
  expect_equal(formula_details$response, "log(y)")
  expect_equal(formula_details$xs_variables, "x")
  expect_equal(formula_details$xf_variables, "t")
})
