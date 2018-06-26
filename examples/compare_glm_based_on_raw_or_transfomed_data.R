library("breakDown")
library("randomForest")
library("DALEX")
library("xspliner")
library("dplyr")

data <- HR_data
logit <- function(x) exp(x)/(1+exp(x))
HR_rf_model <- randomForest(factor(left)~., data = data, ntree = 100)

explainer_rf  <- explain(HR_rf_model, data = data, y = data$left,
                         predict_function = function(model, newdata, ...)
                           predict(model, newdata, type = "prob")[,2])

HR_glm_model <- glm(left~., data = data, family = "binomial")
HR_spline_model <- build_spline_model(explainer_rf, data, "left")

data$pred_glm <- round(logit(predict(HR_glm_model, data)))
data$pred_spline <- round(logit(predict(HR_spline_model, data)))

# Acc:
sum(data$pred_glm == data$left) / nrow(data)
sum(data$pred_spline == data$left) / nrow(data)


