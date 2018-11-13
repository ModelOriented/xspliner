set.seed(13)
N <- 250
X1 <- runif(N)
X2 <- runif(N)
X3 <- runif(N)
X4 <- runif(N)
X5 <- runif(N)

f <- function(x1, x2, x3, x4, x5) {
  ((x1-0.5)*2)^2-0.5 + sin(x2*10) + x3^6 + (x4-0.5)*2 + abs(2*x5-1)
}
y <- f(X1, X2, X3, X4, X5)

library(randomForest)
library(DALEX)
library(e1071)
library(rms)

df <- data.frame(y, X1, X2, X3, X4, X5)

model_rf <- randomForest(y~., df)
model_svm <- svm(y ~ ., df)
model_lm <- lm(y ~ ., df)

# thanks to https://github.com/pbiecek/DALEX/issues/24
## important setup step required for use of rms functions
dd <- datadist(df)
options(datadist="dd")
## add rcs terms to linear model
## this is a very convenient, objective way to account for non-linearity
## still a "linear" model because terms are linear combinations (additive)
library(mgcv)
model_rms <- rms::ols(y ~ rcs(X1) + rcs(X2) + rcs(X3) + rcs(X4) + rcs(X5), df)
model_gam <- mgcv::gam(data = df, y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5))

ex_rf <- explain(model_rf)
ex_svm <- explain(model_svm)
ex_lm <- explain(model_lm)
ex_rms <- explain(model_rms, label = "rms", data = df[, -1], y = df$y)
ex_tr <- explain(model_lm, data = df[,-1],
                 predict_function = function(m, x) f(x[,1], x[,2], x[,3], x[,4], x[,5]),
                 label = "True Model")
ex_gam <- explain(model_gam, label = "gam", data = df[, -1], y = df$y)

library(ggplot2)
plot(single_variable(ex_rf, "X1"),
     single_variable(ex_svm, "X1"),
     single_variable(ex_lm, "X1"),
     single_variable(ex_rms, "X1"),
     single_variable(ex_tr, "X1"),
     single_variable(ex_gam, "X1")) +
  ggtitle("Responses for X1. Truth: y ~ (2*x1 - 1)^2")
