context("test-utils-approximations")

test_that("formula for spline approximation is prepared correctly", {
  env <- new.env()
  expect_equal(build_approximation_formula("y", "x", env), as.formula("y ~ s(x)", env = env))

  expect_equal(build_approximation_formula("y", "x", env, k = 6), as.formula("y ~ s(x, k = 6)", env = env))

  params = list(predictor = "x", response = "y", env = env, k = 6)
  expect_equal(do.call(build_approximation_formula, params), as.formula("y ~ s(x, k = 6)", env = env))
})

test_that("spline approximation is made correctly with mgcv::gam and mgcv::s", {
  env <- new.env()
  data <- data.frame(x = 1:10, y = 11:20)

  with_mock(
    build_approximation_formula = function(response, predictor, env, ...) as.formula("y ~ s(x)", env = env),
    expect_true("gam" %in% class(approx_with_spline(data, "y", "x", env)))
  )

  with_mock(
    build_approximation_formula = function(response, predictor, env, ...) as.formula("y ~ s(x)", env = env),
    expect_equal(approx_with_spline(data, "y", "x", env)$formula, as.formula("y ~ s(x)", env = env))
  )

  with_mock(
    build_approximation_formula = function(response, predictor, env, ...) as.formula("y ~ s(x, k = 6)", env = env),
    expect_equal(approx_with_spline(data, "y", "x", env, k = 6)$smooth[[1]]$bs.dim, 6)
  )

  with_mock(
    build_approximation_formula = function(response, predictor, env, ...) as.formula("y ~ s(x, k = 6)", env = env),
    expect_equal(approx_with_spline(data, "y", "x", env, k = 6)$formula, as.formula("y ~ s(x, k = 6)", env = env))
  )
})

test_that("monotonic spline approximation is made correctly with mgcv::gam and mgcv::s", {
  env <- new.env()
  data <- data.frame(x = 1:10, y = sort(rnorm(10)))
  suppressWarnings({
    with_mock(
      build_approximation_formula = function(response, predictor, env, ...)
        as.formula("y ~ s(x)", env = env),
      expect_true("gam" %in% class(approx_with_monotonic_spline(data, "y", "x", env, TRUE)))
    )

    with_mock(
      build_approximation_formula = function(response, predictor, env, ...)
        as.formula("y ~ s(x)", env = env),
      expect_equal(
        approx_with_monotonic_spline(data, "y", "x", env, TRUE)$formula,
        as.formula("y ~ s(x)", env = env))
    )

    with_mock(
      build_approximation_formula = function(response, predictor, env, ...)
        as.formula("y ~ s(x, k = 6)", env = env),
      expect_equal(approx_with_monotonic_spline(data, "y", "x", env, TRUE, k = 6)$smooth[[1]]$bs.dim, 6)
    )

    with_mock(
      build_approximation_formula = function(response, predictor, env, ...)
        as.formula("y ~ s(x, k = 6)", env = env),
      expect_equal(
        approx_with_monotonic_spline(data, "y", "x", env, TRUE, k = 6)$formula,
        as.formula("y ~ s(x, k = 6)", env = env))
    )
  })
})

test_that("prepare_transition_params_pdp correctly gets pdp response and its approximation", {
  formula <-  log(y) ~ xs(x, effect = list(type = "pdp")) * z + xf(t) + log(a)
  formula_metadata <- get_formula_metadata(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "fM"),
                  transition = list(stat = "GIC", value = 3))
  special_components_details <- collect_specials_metadata(formula_metadata, xs_opts, xf_opts)

  x_var_spline_params <- prepare_transition_params_pdp(formula_metadata, special_components_details$x, blackbox, data)

  expect_equal(names(x_var_spline_params), c("alter", "monotonic", "effect_data", "predictor", "response", "env"))
  expect_true("partial" %in% class(x_var_spline_params$effect_data))
  expect_equal(colnames(x_var_spline_params$effect_data), c("x", "yhat"))
})

test_that("prepare_transition_params_ale correctly gets ale response and its approximation", {
  formula <-  log(y) ~ xs(x, effect = list(type = "ale")) * z + xf(t) + log(a)
  formula_metadata <- get_formula_metadata(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "fM"),
                  transition = list(stat = "GIC", value = 3))
  special_components_details <- collect_specials_metadata(formula_metadata, xs_opts, xf_opts)

  x_var_spline_params <- prepare_transition_params_ale(formula_metadata, special_components_details$x, blackbox, data)

  expect_equal(names(x_var_spline_params), c("alter", "monotonic", "effect_data", "predictor", "response", "env"))
  expect_equal("data.frame", class(x_var_spline_params$effect_data))
  expect_equal(colnames(x_var_spline_params$effect_data), c("x", "yhat"))
})

test_that("get_quantitative_transition correctly gets response and its approximation", {
  formula <-  log(y) ~ xs(x, effect = list(type = "pdp")) * z + xf(t) + log(a)
  formula_metadata <- get_formula_metadata(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "fM"),
                  transition = list(stat = "GIC", value = 3))
  special_components_details <- collect_specials_metadata(formula_metadata, xs_opts, xf_opts)

  x_var_approx_env <- get_quantitative_transition(formula_metadata, special_components_details$x, blackbox, data)

  expect_equal(names(x_var_approx_env), c("effect_outcome", "transition_outcome"))
  expect_true("partial" %in% class(x_var_approx_env$effect_outcome))
  expect_true("glm" %in% class(x_var_approx_env$transition_outcome))
  expect_equal(colnames(x_var_approx_env$effect_outcome), c("x", "yhat"))

  expect_error(get_quantitative_transition(formula_metadata, special_components_details$t, blackbox, data))
})

test_that("get_transitions_outcome correctly gets response and its approximation for all special calls", {
  formula <-  log(y) ~
    xs(x, effect = list(type = "pdp")) * z + xs(t, effect = list(type = "pdp", k = 6)) + log(a)
  formula_metadata <- get_formula_metadata(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "fM"),
                  transition = list(stat = "GIC", value = 3))
  special_components_details <- collect_specials_metadata(formula_metadata, xs_opts, xf_opts)

  vars_approx_env <- get_transitions_outcome(formula_metadata, special_components_details, blackbox, data)

  expect_equal(names(vars_approx_env), c("quantitative", "qualitative"))
  expect_equal(vars_approx_env$qualitative_transition, NULL)
  expect_equal(names(vars_approx_env$quantitative$x), c("effect_outcome", "transition_outcome"))
  expect_equal(names(vars_approx_env$quantitative$t), c("effect_outcome", "transition_outcome"))
  expect_equal(colnames(vars_approx_env$quantitative$x$effect_outcome), c("x", "yhat"))
  expect_equal(colnames(vars_approx_env$quantitative$t$effect_outcome), c("t", "yhat"))

})

test_that("build_xs_function correctly use gam object for prediction", {
  set.seed(123)
  x = rnorm(10)
  y = 2 * x + rnorm(10, 0, 0.001)
  data <- data.frame(x, y)
  quantitative_transition <- list()
  quantitative_transition$transition_outcome <- mgcv::gam(y ~ x, data = data)
  quantitative_transition$effect_outcome <- data.frame(x, y)
  xs <- build_xs_function(quantitative_transition, "x")
  expect_equivalent(round(xs(1)), 2)
  expect_equal(attr(xs, "variable_range"), range(quantitative_transition$effect_outcome$x))

  expect_error(suppressWarnings(build_xs_function(quantitative_transition, "t")(1)))

})

# (todo)
# test_that("build_xs_function correctly use gam object for prediction", {
# })

test_that("is_lm_better_than_xs correctly chooses model", {
  x = 1:10
  y = 2 * x + rnorm(10, 0, 0.001)
  data <- data.frame(x, y)
  approx_fun <- sin
  expect_true(is_lm_better_than_xs(data, "y", "x", approx_fun, gaussian(), aic))

  y = x ^ 2 + rnorm(10, 0, 0.001)
  data <- data.frame(x, y)
  approx_fun <- function(x) x ^ 2
  expect_false(is_lm_better_than_xs(data, "y", "x", approx_fun, gaussian(), aic))
})

test_that("correct_improved_components changes calls", {
  formula <-  y ~ xs(x, effect = list(type = "pdp")) + log(a)
  formula_metadata <- get_formula_metadata(formula, c("y" ,"x", "a"))
  x = 1:10
  data <- data.frame(y = 2 * x + rnorm(10, 0, 0.1), x, a = rexp(10))
  xs_opts <- list(effect = list(type = "pdp"),
                  transition = list(alter = "always", monotonic = "not"))
  xf_opts <- list(effect = list(type = "fM"),
                  transition = list(stat = "GIC", value = 3))
  special_components_details <- collect_specials_metadata(formula_metadata, xs_opts, xf_opts)

  expect_equal(
    correct_improved_components(special_components_details, "x"),
    special_components_details)
  expect_equal(
    correct_improved_components(special_components_details, "y")$x$new_call, "x")
})
