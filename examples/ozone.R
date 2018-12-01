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

library(tidyr)
library(dplyr)
data <- data.frame(gamm, glmm, rf, xs) %>%
  arrange(rf) %>%
  mutate(number = 1:nrow(.)) %>%
  gather(key = "type", value = "result", -number)

ggplot(data, aes(number, type)) +
  geom_tile(aes(fill = result))

tss <- function(model, data, var) {
  fitted <- predict(model, newdata = data, type = "response")
  mean((fitted - data[[var]])^2)
}
tss(model_rf, ozone, "Ozone")
tss(model_xs, ozone, "Ozone")
tss(model_glm, ozone, "Ozone")
tss(model_gam, ozone, "Ozone")

coef(model_xs)
summary(model_xs)
summary(model_glm)
