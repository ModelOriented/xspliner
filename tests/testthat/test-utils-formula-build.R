context("test-utils-formula-build")

test_that("raw variable names correspond to additive components", {
  type <- "pdp"
  formula <- log(y) ~
    xs(x, effect = list(type = type)) * z + xf(t) + w ^ 2 + I(z ^ 2)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), w = rexp(10))
  formula_terms <- terms.formula(formula, specials = c("xs", "xf"))

  # (todo) test shows that functionality doesn't work when variables are duplicated
  expect_equal(
    get_formula_single_components(formula_terms),
    c("xs(x, effect = list(type = type))", "z", "xf(t)", "w", "I(z^2)")
  )

  data$a <- rnorm(10)
  data$b <- rnorm(10)
  formula_2 <- log(y) ~
    xs(x, effect = list(type = type)) * log(z) + xf(t) + w ^ 2 + log(a) + sqrt(exp(b))
  formula_terms <- terms.formula(formula_2, specials = c("xs", "xf"))
  expect_equal(
    get_formula_single_components(formula_terms),
    c("xs(x, effect = list(type = type))", "log(z)", "xf(t)", "w", "log(a)", "sqrt(exp(b))")
  )
})

test_that("formula variables are extracted properly", {
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), w = rexp(10))
  is_centered <- FALSE
  formula_1 <- y ~ log(x) + xs(z, transition = list(k = 6))
  formula_2 <- log(y) ~
    xs(x, transition = list(k = 6), effect = list(type = "pdp", center = is_centered)) * z + xf(t) + w ^ 2

  expect_equal(extract_formula_var_names(formula_1, data), c("y", "x", "z"))
  expect_equal(extract_formula_var_names(formula_2, data), c("y", "x", "z", "t", "w"))
  expect_error(suppressWarnings(extract_formula_var_names(formula_1)))
  expect_error(suppressWarnings(extract_formula_var_names(formula_2)))

  type <- "pdp"
  x <- rnorm(10)
  y <- x + rnorm(10, 0, 0.001)
  z <- x ^ 2 + rnorm(10, 0, 0.001)
  formula_3 <- y ~ log(x, effect = list(type = type)) + xs(z, transition = list(k = 6))
  expect_equal(suppressWarnings(extract_formula_var_names(formula_3)), c("y", "x", "z"))

  # (todo) should z be duplicated?
  type <- "pdp"
  formula_4 <- log(y) ~
    xs(x, effect = list(type = type)) * z + xf(t) + w ^ 2 + z ^ 2
  expect_equal(extract_formula_var_names(formula_4, data), c("y", "x", "z", "t", "w"))
})

test_that("get_formula_metadata returns formula info in correct form", {
  # get_formula_metadata depends on other functions -> use with_mock
  is_centered <- FALSE
  formula <- log(y) ~
    xs(x, transition = list(k = 6), effect = list(type = "pdp", center = is_centered)) * z + xf(t) + w ^ 2
  variable_names <- c("y", "x", "z", "t", "w")
  formula_metadata <- get_formula_metadata(formula, variable_names)

  expect_equal(formula_metadata$formula, formula)
  expect_equal(formula_metadata$response, "y")
  expect_equal(formula_metadata$predictors, c("x", "z", "t", "w"))
  expect_equal(formula_metadata$lhs, "log(y)")
  expect_equal(formula_metadata$xs_variables, "x")
  expect_equal(formula_metadata$xf_variables, "t")
  expect_identical(
    formula_metadata$additive_components,
    c("xs(x, transition = list(k = 6), effect = list(type = \"pdp\", center = is_centered))",
      "z", "xf(t)", "w")
  )
})

test_that("clear_special_component transforms component into simple form", {
  expect_equal(clear_special_component("log(x)"), "log(x)")
  expect_equal(clear_special_component("x ^ 2"), "x ^ 2")
  expect_equal(clear_special_component("I(x ^ 2)"), "I(x ^ 2)")
  expect_equal(clear_special_component("xs(x)"), "xs(x)")
  expect_equal(clear_special_component("xf(x)"), "xf(x)")
  expect_equal(clear_special_component("xs(var_name)"), "xs(var_name)")
  expect_equal(clear_special_component("xs(x, effect = list(type = 'pdp'))"), "xs(x)")
  expect_equal(clear_special_component("xs(x ^ 2)"), "xs(x ^ 2)")
  expect_equal(clear_special_component("xs(x ^ 2, effect = list(type = 'pdp'))"), "xs(x ^ 2)")
})

test_that("formula component parameters are extracted correctly", {
  env_0 <- new.env()
  env_1 <- new.env() # parent.frame as parent env
  env_1$a <- 1
  env_1$type <- "foo"
  env_2 <- as.environment(list(a = 1, type = "pdp")) # empty env as parent env
  additive_component_1 <- "xf(x)"
  additive_component_2 <- "xs(x, effect = list(type = 'ale'))"
  additive_component_3 <- "xs(x, effect = list(type = type))"
  additive_component_4 <- "xs(x, effect = list(type = type), transition = list(k = a))"
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "ice"),
                  transition = list(alter = "never", stat = "GIC", value = 3))

  expect_identical(
    get_component_params(additive_component_1, xs_opts, xf_opts, env_0),
    list(transition = xf_opts$transition, effect = xf_opts$effect)
  )
  expect_identical(
    get_component_params(additive_component_2, xs_opts, xf_opts, env_0),
    list(transition = xs_opts$transition, effect = list(type = "ale"))
  )
  expect_error(
    get_component_params(additive_component_3, xs_opts, xf_opts, env_0)
  )

  expect_identical(
    get_component_params(additive_component_3, xs_opts, xf_opts, env_1),
    list(transition = xs_opts$transition, effect = list(type = "foo"))
  )
  expect_identical(
    get_component_params(additive_component_4, xs_opts, xf_opts, env_1),
    list(transition = list(k = 1, alter = "always", monotonic = "not"), effect = list(type = "foo"))
  )

  expect_error(
    get_component_params(additive_component_3, xs_opts, xf_opts, env_2)
  )
  expect_error(
    get_component_params(additive_component_4, xs_opts, xf_opts, env_2)
  )

})

test_that("additive component details are extracted and stored correctly", {
  env_0 <- new.env()
  env_1 <- new.env()
  env_1$a <- 1
  env_1$type <- "foo"
  additive_component_1 <- "xf(x)"
  additive_component_2 <- "xs(x, effect = list(type = 'pdp'))"
  additive_component_3 <- "xs(x, effect = list(type = type))"
  additive_component_4 <- "xs(x, effect = list(type = type), transition = list(k = a))"
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "ice"),
                  transition = list(alter = "newer", stat = "GIC", value = 3))

  expect_equal(
    get_special_component_metadata("x", additive_component_1, xs_opts, xf_opts, env_0),
    list(var = "x", call = "xf(x)", new_call = "xf(x)",
         call_fun = "xf", transition = xf_opts$transition, effect = xf_opts$effect)
  )
  expect_equal(
    get_special_component_metadata("x", additive_component_2, xs_opts, xf_opts, env_0),
    list(var = "x", call = "xs(x, effect = list(type = 'pdp'))",
         new_call = "xs(x)", call_fun = "xs", transition = xs_opts$transition, effect = list(type = "pdp"))
  )
  expect_error(
    get_special_component_metadata("x", additive_component_3, xs_opts, xf_opts, env_0)
  )
  expect_equal(
    get_special_component_metadata("x", additive_component_3, xs_opts, xf_opts, env_1),
    list(var = "x", call = "xs(x, effect = list(type = type))", new_call = "xs(x)",
         call_fun = "xs", transition = xs_opts$transition, effect = list(type = "foo"))
  )
  expect_equal(
    get_special_component_metadata("x", additive_component_4, xs_opts, xf_opts, env_1),
    list(var = "x", call = "xs(x, effect = list(type = type), transition = list(k = a))", new_call = "xs(x)",
         call_fun = "xs", transition = list(k = 1, alter = "always", monotonic = "not"), effect = list(type = "foo"))
  )
})

test_that("collect_specials_metadata properly identifies special components and extracts its details", {
  formula_metadata <- list(
    formula = log(y) ~ xs(x, effect = list(type = "type")) * z + xf(t) + log(a) + xs(d),
    response = "y", predictors = c("x", "z", "t", "a", "d"),
    lhs = "log(y)",
    rhs = "xs(x, effect = list(type = \"type\")) * z + xf(t) + log(a) + xs(d)",
    additive_components = c("xs(x, effect = list(type = \"type\"))", "z", "xf(t)", "log(a)", "xs(d)"),
    xs_variables = c("x", "d"), xf_variables = "t", xs_variables_idx = c(1, 5), xf_variables_idx = 3)
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "ice"),
                  transition = list(stat = "GIC", value = 3))

  special_components_info <- collect_specials_metadata(formula_metadata, xs_opts, xf_opts)
  expect_equal(names(special_components_info), c("x", "d", "t"))
  expect_equal(
    special_components_info$x,
    list(var = "x", call = "xs(x, effect = list(type = \"type\"))",
         new_call = "xs(x)", call_fun = "xs", transition = xs_opts$transition, effect = list(type = "type"))
  )

})


test_that("formula last string form is correct", {
  formula_metadata <- list(
    formula = log(y) ~ xs(x, effect = list(type = "type")) * z + xf(t) + log(a),
    response = "y", predictors = c("x", "z", "t", "a"),
    lhs = "log(y)",
    rhs = "xs(x, effect = list(type = \"type\")) * z + xf(t) + log(a)",
    additive_components = c("xs(x, effect = list(type = \"type\"))", "z", "xf(t)", "log(a)"),
    xs_variables = "x", xf_variables = "t", xs_variables_idx = 1, xf_variables_idx = 3)

  special_component_info <- list(
    x = list(var = "x", call = "xs(x, effect = list(type = \"type\"))",
             new_call = "xs(x)", transition = list(alter = "auto"), effect = list(type = "type")),
    t = list(var = "t", call = "xf(t)",
             new_call = "xf(t)", transition = list(stat = "GIC"), effect = list(type = "ice")))

  expect_equal(transform_formula_chr(formula_metadata, special_component_info), "log(y) ~ xs(x) * z + xf(t) + log(a)")

})

test_that("transformed_formula_object returns correct formula form and environment", {
  formula <- log(y) ~ xs(x, effect = list(type = "pdp"), transition = list(alter = "auto")) * z +
    xf(t, effect = list(type = "ice")) + log(a)
  formula_metadata <- get_formula_metadata(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10),
                     t = factor(sample(LETTERS[1:2], 10, replace = TRUE)), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "ice"),
                  transition = list(alter = "never", stat = "GIC", value = 3))

  transformed_formula <- transformed_formula_object(formula_metadata, blackbox, data, gaussian(), xs_opts, xf_opts, aic)
  env <- attr(transformed_formula, ".Environment")

  expect_true(inherits(transformed_formula, "formula"))
  expect_true(all(c("xf", "xs") %in% ls(env)))
  expect_equal(length(env$xs), 1)
  expect_equal(length(env$xf), 1)
  compare_stat <- function(lm_model) 1
  attr(compare_stat, "higher-better") <- TRUE

  transformed_formula <- transformed_formula_object(formula_metadata, blackbox, data, gaussian(),
                                                    xs_opts, xf_opts, compare_stat)
  env <- attr(transformed_formula, ".Environment")

  expect_true(inherits(transformed_formula, "formula"))
  expect_true(all(c("xf", "xs") %in% ls(env)))
  expect_equal(length(env$xs), 1)
  expect_equal(length(env$xf), 1)
  expect_equal(format(transformed_formula), "log(y) ~ x * z + t + log(a)")
})
