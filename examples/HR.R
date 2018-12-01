library(breakDown)
library(pdp)
library(randomForest)
library(xspliner)
library(ggplot2)

# randomForest
HR <- breakDown::HR_data
HR$left <- as.factor(HR$left)
model_rf <- randomForest(left ~ satisfaction_level + last_evaluation + average_montly_hours + sales,
                         data = HR, ntree = 100)
model_xs <- xspline(left ~ satisfaction_level + last_evaluation + average_montly_hours + sales,
                    model_rf, link = "logit", family = binomial(), consider = "all")

model_xs
summary(model_xs, "sales")
plot(model_xs, "satisfaction_level")

rf <- predict(model_rf, newdata = HR, type = "prob")[, 2]
xs <- predict(model_xs, newdata = HR, type = "response")
table(xs, rf)
library(tidyr)
data <- data.frame(rf, xs, number = 1:length(xs)) %>%
  gather(key = "type", value = "result", -number)
#data$result <- factor(data$result)
ggplot(data, aes(type, number)) +
  geom_tile(aes(fill = result))


## boston
data(boston)
boston[, c("lon", "lat", "rad")] <- NULL
rf_model <- randomForest(chas ~ ., data = boston)
xs_model <- xspline(rf_model, link = "logit", family = binomial())

rf <- predict(rf_model, newdata = boston, type = "prob")[, 2]
xs <- predict(xs_model, newdata = boston, type = "response")
library(tidyr)
data <- data.frame(rf, xs, number = 1:length(xs)) %>%
  gather(key = "type", value = "result", -number)
#data$result <- factor(data$result)
ggplot(data, aes(type, number)) +
  geom_tile(aes(fill = result))

data_resp <- data.frame(xf = round(rf), xs = round(xs)) %>%
  dplyr::arrange(rf) %>%
  dplyr::mutate(number = 1:nrow(.)) %>%
  gather(key = "type", value = "result", -number)
data_resp$result <- factor(data_resp$result)
ggplot(data_resp, aes(type, number)) +
  geom_tile(aes(fill = result))
table(round(rf), round(xs))

data(boston)
boston[, c("lon", "lat", "rad")] <- NULL
rf_model <- randomForest(cmedv ~ ., data = boston[, c("cmedv", "lstat", "rm")])
xs_model <- xspline(rf_model, data = boston, form = "multiplicative")
glm_model <- glm(cmedv ~ ., data = boston[, c("cmedv", "lstat", "rm")], family = gaussian())

rf <- predict(rf_model, newdata = boston, type = "response")
xs <- predict(xs_model, newdata = boston, type = "response")
xg <- predict(glm_model, newdata = boston, type = "response")
library(tidyr)
data <- data.frame(rf, xs, number = 1:length(xs)) %>%
  gather(key = "type", value = "result", -number)

ggplot(data, aes(number, type)) +
  geom_tile(aes(fill = result))

tss <- function(model, data) {
  fitted <- predict(model, newdata = boston, type = "response")
  mean((fitted - data$cmedv)^2)
}

tss(rf_model, boston)
tss(xs_model, boston)
tss(glm_model, boston)
