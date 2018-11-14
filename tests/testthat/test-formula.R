context("test-formula")

test_that("raw variable names correspond to additive components", {
  type <- "pdp"
  formula <- log(y) ~
    xs(x, method_opts = list(type = type)) * z + xf(t) + w ^ 2 + I(z ^ 2)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), w = rexp(10))
  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))

  # (todo) test shows that functionality doesn't work when variables are duplicated
  expect_equal(
    get_formula_raw_components(formula_terms),
    c("xs(x, method_opts = list(type = type))", "z", "xf(t)", "w", "I(z^2)")
  )

  data$a <- rnorm(10)
  data$b <- rnorm(10)
  formula_2 <- log(y) ~
    xs(x, method_opts = list(type = type)) * log(z) + xf(t) + w ^ 2 + log(a) + sqrt(exp(b))
  formula_terms <- terms.formula(formula_2, specials = c("xs", "xf"))
  expect_equal(
    get_formula_raw_components(formula_terms),
    c("xs(x, method_opts = list(type = type))", "log(z)", "xf(t)", "w", "log(a)", "sqrt(exp(b))")
  )
})

test_that("formula variables are extracted properly", {
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), w = rexp(10))
  is_centered <- FALSE
  formula_1 <- y ~ log(x) + xs(z, spline_opts = list(k = 6))
  formula_2 <- log(y) ~
    xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp", center = is_centered)) * z + xf(t) + w ^ 2

  expect_equal(extract_formula_var_names(formula_1, data), c("y", "x", "z"))
  expect_equal(extract_formula_var_names(formula_2, data), c("y", "x", "z", "t", "w"))
  expect_error(suppressWarnings(extract_formula_var_names(formula_1)))
  expect_error(suppressWarnings(extract_formula_var_names(formula_2)))

  type <- "pdp"
  x <- rnorm(10)
  y <- x + rnorm(10, 0, 0.001)
  z <- x ^ 2 + rnorm(10, 0, 0.001)
  formula_3 <- y ~ log(x, method_opts = list(type = type)) + xs(z, spline_opts = list(k = 6))
  expect_equal(suppressWarnings(extract_formula_var_names(formula_3)), c("y", "x", "z"))

  # (todo) should z be duplicated?
  type <- "pdp"
  formula_4 <- log(y) ~
    xs(x, method_opts = list(type = type)) * z + xf(t) + w ^ 2 + z ^ 2
  expect_equal(extract_formula_var_names(formula_4, data), c("y", "x", "z", "t", "w"))
})

test_that("get_formula_details returns formula info in correct form", {
  # get_formula_details depends on other functions -> use with_mock
  is_centered <- FALSE
  formula <- log(y) ~
    xs(x, spline_opts = list(k = 6), method_opts = list(type = "pdp", center = is_centered)) * z + xf(t) + w ^ 2
  variable_names <- c("y", "x", "z", "t", "w")
  formula_details <- get_formula_details(formula, variable_names)

  expect_equal(formula_details$formula, formula)
  expect_equal(formula_details$raw_response_name, "y")
  expect_equal(formula_details$raw_predictor_names, c("x", "z", "t", "w"))
  expect_equal(formula_details$response, "log(y)")
  expect_equal(formula_details$xs_variables, "x")
  expect_equal(formula_details$xf_variables, "t")
  expect_identical(
    formula_details$additive_components,
    c("xs(x, spline_opts = list(k = 6), method_opts = list(type = \"pdp\", center = is_centered))",
      "z", "xf(t)", "w")
  )
})

test_that("prepare_call transforms component into simple form", {
  expect_equal(prepare_call("log(x)"), "log(x)")
  expect_equal(prepare_call("x ^ 2"), "x ^ 2")
  expect_equal(prepare_call("I(x ^ 2)"), "I(x ^ 2)")
  expect_equal(prepare_call("xs(x)"), "xs(x)")
  expect_equal(prepare_call("xf(x)"), "xf(x)")
  expect_equal(prepare_call("xs(var_name)"), "xs(var_name)")
  expect_equal(prepare_call("xs(x, method_opts = list(type = 'pdp'))"), "xs(x)")
  expect_equal(prepare_call("xs(x ^ 2)"), "xs(x ^ 2)")
  expect_equal(prepare_call("xs(x ^ 2, method_opts = list(type = 'pdp'))"), "xs(x ^ 2)")
})

test_that("formula component parameters are extracted correctly", {
  env_0 <- new.env()
  env_1 <- new.env() # parent.frame as parent env
  env_1$a <- 1
  env_1$type <- "pdp"
  env_2 <- as.environment(list(a = 1, type = "pdp")) # empty env as parent env
  additive_component_1 <- "xf(x)"
  additive_component_2 <- "xs(x, method_opts = list(type = 'pdp'))"
  additive_component_3 <- "xs(x, method_opts = list(type = type))"
  additive_component_4 <- "xs(x, method_opts = list(type = type), spline_opts = list(k = a))"

  expect_identical(
    get_component_params(additive_component_1, env_0),
    list(spline_opts = NULL, method_opts = NULL)
  )
  expect_identical(
    get_component_params(additive_component_2, env_0),
    list(spline_opts = NULL, method_opts = list(type = "pdp"))
  )
  expect_error(
    get_component_params(additive_component_3, env_0)
  )

  expect_identical(
    get_component_params(additive_component_3, env_1),
    list(spline_opts = NULL, method_opts = list(type = "pdp"))
  )
  expect_identical(
    get_component_params(additive_component_4, env_1),
    list(spline_opts = list(k = 1), method_opts = list(type = "pdp"))
  )

  expect_error(
    get_component_params(additive_component_3, env_2)
  )
  expect_error(
    get_component_params(additive_component_4, env_2)
  )

})

test_that("additive component details are extracted and stored correctly", {
  env_0 <- new.env()
  env_1 <- new.env()
  env_1$a <- 1
  env_1$type <- "pdp"
  additive_component_1 <- "xf(x)"
  additive_component_2 <- "xs(x, method_opts = list(type = 'pdp'))"
  additive_component_3 <- "xs(x, method_opts = list(type = type))"
  additive_component_4 <- "xs(x, method_opts = list(type = type), spline_opts = list(k = a))"

  expect_equal(
    special_component_details("x", additive_component_1, env_0),
    list(var = "x", call = "xf(x)", new_call = "xf(x)",
         spline_opts = NULL, method_opts = NULL)
  )
  expect_equal(
    special_component_details("x", additive_component_2, env_0),
    list(var = "x", call = "xs(x, method_opts = list(type = 'pdp'))",
         new_call = "xs(x)", spline_opts = NULL, method_opts = list(type = "pdp"))
  )
  expect_error(
    special_component_details("x", additive_component_3, env_0)
  )
  expect_equal(
    special_component_details("x", additive_component_3, env_1),
    list(var = "x", call = "xs(x, method_opts = list(type = type))",
         new_call = "xs(x)", spline_opts = NULL, method_opts = list(type = "pdp"))
  )
  expect_equal(
    special_component_details("x", additive_component_4, env_1),
    list(var = "x", call = "xs(x, method_opts = list(type = type), spline_opts = list(k = a))",
         new_call = "xs(x)", spline_opts = list(k = 1), method_opts = list(type = "pdp"))
  )
})
