context("test-model")

test_that("xspline return object with correct attributes and values", {
  formula <- y ~ xs(x, effect = list(type = "pdp")) * z
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  model <- xspline(formula, blackbox, data)

  expect_true("glm" %in% class(model))
  expect_equal(
    ls(environment(model$formula)),
    c("qualitative_transitions", "quantitative_transitions", "response", "xf", "xf_functions", "xs", "xs_functions"))
  expect_equal(deparse(model$formula), "y ~ xs(x) * z")
})

test_that("xspline correctly predicts values", {
  set.seed(123)
  formula <- y ~ xs(x, effect = list(type = "pdp")) * z
  data <- data.frame(y = rnorm(10, 2), x = rnorm(10), z = rnorm(10, 10))
  blackbox <- randomForest::randomForest(y ~ ., data)
  model <- xspline(formula, blackbox, data)
  xs <- environment(model$formula)$xs
  x <-  1
  z <- 2
  predicted <- predict(model, data.frame(x, z))
  coeffs <- model$coefficients

  expect_length(predicted, 1)
  expect_equivalent(predicted, coeffs[1] + coeffs[2] * xs(x) + coeffs[3] * z + coeffs[4] * xs(x) * z)

})
