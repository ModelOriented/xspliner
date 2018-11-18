context("test-model")

test_that("xp_gam return object with correct attributes and values", {
  formula <- y ~ xs(x, method_opts = list(type = "pdp")) * z
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  model <- xp_gam(formula, blackbox, data)

  expect_true("gam" %in% class(model))
  expect_equal(ls(environment(model$formula)), c("xf", "xs"))
  expect_equal(deparse(model$formula), "y ~ xs(x) * z")
})

test_that("xp_gam correctly predicts values", {
  set.seed(123)
  formula <- y ~ xs(x, method_opts = list(type = "pdp")) * z
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  model <- xp_gam(formula, blackbox, data)
  xs <- environment(model$formula)$xs
  x <-  1
  z <- 2
  predicted <- xp_gam_predict(model, data.frame(x, z))
  coeffs <- model$coefficients

  expect_length(predicted, 1)
  expect_equal(predicted, coeffs[1] + coeffs[2] * xs(x) + coeffs[3] * z + coeffs[4] * xs(x) * z)

})
