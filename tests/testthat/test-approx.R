context("test-approx")

test_that("formula for spline approximation is prepared correctly", {
  env <- new.env()
  expect_equal(get_spline_formula("y", "x", env), as.formula("y ~ s(x)", env = env))

  expect_equal(get_spline_formula("y", "x", env, k = 6), as.formula("y ~ s(x, k = 6)", env = env))

  params = list(pred_var = "x", response_var = "y", env = env, k = 6)
  expect_equal(do.call(get_spline_formula, params), as.formula("y ~ s(x, k = 6)", env = env))
})

test_that("spline approximation is made correctly with mgcv::gam and mgcv::s", {
  env <- new.env()
  data <- data.frame(x = 1:10, y = 11:20)

  with_mock(
    get_spline_formula = function(response_var, pred_var, env, ...) as.formula("y ~ s(x)", env = env),
    expect_true("gam" %in% class(approx_with_spline(data, "y", "x", env)))
  )

  with_mock(
    get_spline_formula = function(response_var, pred_var, env, ...) as.formula("y ~ s(x)", env = env),
    expect_equal(approx_with_spline(data, "y", "x", env)$formula, as.formula("y ~ s(x)", env = env))
  )

  with_mock(
    get_spline_formula = function(response_var, pred_var, env, ...) as.formula("y ~ s(x, k = 6)", env = env),
    expect_equal(approx_with_spline(data, "y", "x", env, k = 6)$smooth[[1]]$bs.dim, 6)
  )

  with_mock(
    get_spline_formula = function(response_var, pred_var, env, ...) as.formula("y ~ s(x, k = 6)", env = env),
    expect_equal(approx_with_spline(data, "y", "x", env, k = 6)$formula, as.formula("y ~ s(x, k = 6)", env = env))
  )
})

test_that("monotonic spline approximation is made correctly with mgcv::gam and mgcv::s", {
  env <- new.env()
  data <- data.frame(x = 1:10, y = sort(rnorm(10)))
  suppressWarnings({
    with_mock(
      get_spline_formula = function(response_var, pred_var, env, ...)
        as.formula("y ~ s(x)", env = env),
      expect_true("gam" %in% class(approx_with_monotonic_spline(data, "y", "x", env, TRUE)))
    )

    with_mock(
      get_spline_formula = function(response_var, pred_var, env, ...)
        as.formula("y ~ s(x)", env = env),
      expect_equal(
        approx_with_monotonic_spline(data, "y", "x", env, TRUE)$formula,
        as.formula("y ~ s(x)", env = env))
    )

    with_mock(
      get_spline_formula = function(response_var, pred_var, env, ...)
        as.formula("y ~ s(x, k = 6)", env = env),
      expect_equal(approx_with_monotonic_spline(data, "y", "x", env, TRUE, k = 6)$smooth[[1]]$bs.dim, 6)
    )

    with_mock(
      get_spline_formula = function(response_var, pred_var, env, ...)
        as.formula("y ~ s(x, k = 6)", env = env),
      expect_equal(
        approx_with_monotonic_spline(data, "y", "x", env, TRUE, k = 6)$formula,
        as.formula("y ~ s(x, k = 6)", env = env))
    )
  })
})

test_that("single_component_env_pdp correctly gets pdp response and its approximation", {
  formula <-  log(y) ~ xs(x, method_opts = list(type = "pdp")) * z + xf(t) + log(a)
  formula_details <- get_formula_details(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  special_components_details <- get_special_components_info(formula_details)

  x_var_approx_env <- single_component_env_pdp(formula_details, special_components_details$x, blackbox, data)

  expect_equal(names(x_var_approx_env), c("blackbox_response_obj", "blackbox_response_approx"))
  expect_true("partial" %in% class(x_var_approx_env$blackbox_response_obj))
  expect_true("gam" %in% class(x_var_approx_env$blackbox_response_approx))
  expect_equal(colnames(x_var_approx_env$blackbox_response_obj), c("x", "yhat"))
})

test_that("single_component_env_ale correctly gets ale response and its approximation", {
  formula <-  log(y) ~ xs(x, method_opts = list(type = "ale")) * z + xf(t) + log(a)
  formula_details <- get_formula_details(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  special_components_details <- get_special_components_info(formula_details)

  x_var_approx_env <- single_component_env_ale(formula_details, special_components_details$x, blackbox, data)

  expect_equal(names(x_var_approx_env), c("blackbox_response_obj", "blackbox_response_approx"))
  expect_equal(class(x_var_approx_env$blackbox_response_obj), "data.frame")
  expect_true("gam" %in% class(x_var_approx_env$blackbox_response_approx))
  expect_equal(colnames(x_var_approx_env$blackbox_response_obj), c("x", "yhat"))
})

test_that("single_component_env correctly gets response and its approximation", {
  formula <-  log(y) ~ xs(x, method_opts = list(type = "pdp")) * z + xf(t) + log(a)
  formula_details <- get_formula_details(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  special_components_details <- get_special_components_info(formula_details)

  x_var_approx_env <- single_component_env(formula_details, special_components_details$x, blackbox, data)

  expect_equal(names(x_var_approx_env), c("blackbox_response_obj", "blackbox_response_approx"))
  expect_true("partial" %in% class(x_var_approx_env$blackbox_response_obj))
  expect_true("gam" %in% class(x_var_approx_env$blackbox_response_approx))
  expect_equal(colnames(x_var_approx_env$blackbox_response_obj), c("x", "yhat"))

  expect_error(single_component_env(formula_details, special_components_details$t, blackbox, data))
})

test_that("get_common_components_env correctly gets response and its approximation for all special calls", {
  formula <-  log(y) ~
    xs(x, method_opts = list(type = "pdp")) * z + xs(t, method_opts = list(type = "pdp", k = 6)) + log(a)
  formula_details <- get_formula_details(formula, c("y" ,"x", "z", "t", "a"))
  set.seed(123)
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10), t = runif(10), a = rexp(10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  special_components_details <- get_special_components_info(formula_details)

  vars_approx_env <- get_common_components_env(formula_details, special_components_details, blackbox, data)

  expect_equal(names(vars_approx_env), c("xs_env", "xf_env"))
  expect_equal(vars_approx_env$xf_env, list())
  expect_equal(names(vars_approx_env$xs_env$x), c("blackbox_response_obj", "blackbox_response_approx"))
  expect_equal(names(vars_approx_env$xs_env$t), c("blackbox_response_obj", "blackbox_response_approx"))
  expect_equal(colnames(vars_approx_env$xs_env$x$blackbox_response_obj), c("x", "yhat"))
  expect_equal(colnames(vars_approx_env$xs_env$t$blackbox_response_obj), c("t", "yhat"))

})

test_that("get_xs_call correctly use gam object for prediction", {
  set.seed(123)
  x = rnorm(10)
  y = 2 * x + rnorm(10, 0, 0.001)
  data <- data.frame(x, y)
  xs_env <- list()
  xs_env$blackbox_response_approx <- mgcv::gam(y ~ x, data = data)
  xs_env$blackbox_response_obj <- data.frame(x, y)
  xs <- get_xs_call(xs_env, "x")
  expect_equivalent(round(xs(1)), 2)
  expect_equal(attr(xs, "variable_range"), range(xs_env$blackbox_response_obj$x))

  expect_error(suppressWarnings(get_xs_call(xs_env, "t")(1)))

})

# (todo)
# test_that("get_xf_call correctly use gam object for prediction", {
# })

