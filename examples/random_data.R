library(e1071)
set.seed(13)
N <- 250
X1 <- runif(N)
X2 <- runif(N)
X3 <- runif(N)
X4 <- runif(N)
X5 <- runif(N)

f <- function(x1, x2, x3, x4, x5) {
  ((x1-0.5)*2)^2-0.5 + sin(x2*10) * x3^6 + (x4-0.5)*2 + abs(2*x5-1) + rnorm(length(x1))
}
y <- f(X1, X2, X3, X4, X5)
df <- data.frame(y, X1, X2, X3, X4, X5)

model_rf <- randomForest::randomForest(y ~ ., data = df)
model_xs <- xspline(model_rf)
model_svm <- svm(y ~ ., df)
model_lm <- lm(y ~ ., df)
model_gam <- mgcv::gam(y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5))

rmse <- function(model, data, var, type = "response") {
  fitted <- predict(model, newdata = data, type = type)
  mean((fitted - data[[var]])^2)
}

rmse(model_rf, data, "y")
rmse(model_xs, data, "y")
rmse(model_svm, data, "y")
rmse(model_lm, data, "y")
rmse(model_gam, data, "y")
