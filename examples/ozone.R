library(randomForest)
library(xspliner)
library(ggplot2)

data(airquality)
ozone <- subset(na.omit(airquality),
                select = c("Ozone", "Solar.R", "Wind", "Temp"))
set.seed(123)
summary(ozone)
model_rf <- randomForest(Ozone ~ ., data = ozone)
model_xs <- xspline(Ozone ~ xs(Solar.R) + xs(Wind) * xs(Temp), model_rf, data = ozone)
model_glm <- glm(Ozone ~ ., data = ozone)
model_gam <- mgcv::gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp), data = ozone)
rf <- predict(model_rf, newdata = ozone)
xs <- predict.glm(model_xs, newdata = ozone, type = "response")
glmm <- predict.glm(model_glm, newdata = ozone, type = "response")
gamm <- as.numeric(mgcv::predict.gam(model_gam, newdata = ozone, type = "response"))

plot(model_xs, model = model_rf, data = ozone, compare_with = list(glm = model_glm, gam = model_gam))

rmse <- function(model, data, var, type = "response") {
  fitted <- predict(model, newdata = data, type = type)
  mean((fitted - data[[var]])^2)
}
rmse(model_rf, ozone, "Ozone")
rmse(model_xs, ozone, "Ozone")
rmse(model_glm, ozone, "Ozone")
rmse(model_gam, ozone, "Ozone")

