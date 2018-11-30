library(breakDown)
library(pdp)
library(randomForest)

data("boston")
model_glm <- glm(chas ~ cmedv + crim + indus + nox, data = boston, family = binomial())
glm_res <- predict(model_glm, newdata = boston)
partial(model_glm, "cmedv", prob = TRUE) %>% plotPartial()

model_rf <- randomForest(chas ~ cmedv + crim + indus + nox, data = boston)
predict(model_rf, newdata = boston)
partial(model_rf, "cmedv") %>% plotPartial()

### bare link case (works fine!)
data("boston")
model_rf <- randomForest(log(ptratio) ~ cmedv + crim, data = boston)
summary(predict(model_rf, newdata = boston))
cmedv_pdp <- pdp::partial(model_rf, "cmedv")
crim_pdp <- pdp::partial(model_rf, "crim")
cmedv_mod <- mgcv::gam(yhat ~ s(cmedv), data = cmedv_pdp)
crim_mod <- mgcv::gam(yhat ~ s(crim), data = crim_pdp)
plot(cmedv_pdp)
lines(cmedv_pdp$cmedv, predict(cmedv_mod, cmedv_pdp))
plot(crim_pdp)
lines(crim_pdp$crim, predict(crim_mod, crim_pdp))

xs_cmedv <- function(vec) {
  data <- data.frame(cmedv = vec)
  predict(cmedv_mod, newdata = data)
}

xs_crim <- function(vec) {
  data <- data.frame(crim = vec)
  predict(crim_mod, newdata = data)
}

glm(log(ptratio) ~ xs_cmedv(cmedv) + xs_crim(crim), data = boston) %>% summary
glm(log(ptratio) ~ cmedv + crim, data = boston) %>% summary

# and with xspline
model <- xspline(model_rf)
summary(model)
plot(model, "cmedv")
plot(model, "crim")

# and bare formula
model <- xspline(
  log(ptratio) ~ xs(cmedv, effect = list(type = "pdp"), spline_opts = list(k = 10)) +
    xs(crim, effect = list(type = "pdp"), spline_opts = list(k = 10)),
  model = model_rf,
  data = boston)
plot(model, "cmedv")
plot(model, "crim")

### classification when using factor (works great)
data("boston")
model_rf <- randomForest(chas ~ cmedv + crim, data = boston)
cmedv_pdp <- pdp::partial(model_rf, "cmedv", prob = FALSE, which.class = 2)
crim_pdp <- pdp::partial(model_rf, "crim", prob = FALSE, which.class = 2)
cmedv_mod <- mgcv::gam(yhat ~ s(cmedv), data = cmedv_pdp)
crim_mod <- mgcv::gam(yhat ~ s(crim), data = crim_pdp)
plot(cmedv_pdp)
lines(cmedv_pdp$cmedv, predict(cmedv_mod, cmedv_pdp))
plot(crim_pdp)
lines(crim_pdp$crim, predict(crim_mod, crim_pdp))

xs_cmedv <- function(vec) {
  data <- data.frame(cmedv = vec)
  predict(cmedv_mod, newdata = data)
}

xs_crim <- function(vec) {
  data <- data.frame(crim = vec)
  predict(crim_mod, newdata = data)
}

glm(chas ~ cmedv + crim, data = boston, family = binomial()) %>% summary
glm(chas ~ cmedv + crim, data = boston, family = binomial()) %>% summary

model_xs <- xspline(model_rf)
environment(model_xs)$quantitative_transitions[["cmedv"]]
plot(model_xs, "cmedv")
plot(model_xs, "crim")

### classification when using family (works great)
data("boston")
model_glm <- glm(chas ~ cmedv + crim, family = binomial(), data = boston)
cmedv_pdp <- pdp::partial(model_glm, "cmedv", prob = FALSE)
crim_pdp <- pdp::partial(model_glm, "crim", prob = FALSE)
cmedv_mod <- mgcv::gam(yhat ~ s(cmedv), data = cmedv_pdp)
crim_mod <- mgcv::gam(yhat ~ s(crim), data = crim_pdp)
plot(cmedv_pdp)
lines(cmedv_pdp$cmedv, predict(cmedv_mod, cmedv_pdp))
plot(crim_pdp)
lines(crim_pdp$crim, predict(crim_mod, crim_pdp))

xs_cmedv <- function(vec) {
  data <- data.frame(cmedv = vec)
  predict(cmedv_mod, newdata = data)
}

xs_crim <- function(vec) {
  data <- data.frame(crim = vec)
  predict(crim_mod, newdata = data)
}

glm(chas ~ xs_cmedv(cmedv) + xs_crim(crim), data = boston, family = binomial()) %>% summary
glm(chas ~ cmedv + crim, data = boston, family = binomial()) %>% summary

###

### other than classification
data(boston)
model_glm <- glm(rad ~ cmedv + crim, data = boston, family = poisson())
cmedv_pdp <- pdp::partial(model_glm, "cmedv", prob = FALSE)
crim_pdp <- pdp::partial(model_glm, "crim", prob = FALSE)
cmedv_mod <- mgcv::gam(yhat ~ s(cmedv), data = cmedv_pdp)
crim_mod <- mgcv::gam(yhat ~ s(crim), data = crim_pdp)
plot(cmedv_pdp)
lines(cmedv_pdp$cmedv, predict(cmedv_mod, cmedv_pdp))
plot(crim_pdp)
lines(crim_pdp$crim, predict(crim_mod, crim_pdp))

model_xs <- xspline(model_glm)
plot(model_xs, "crim")

