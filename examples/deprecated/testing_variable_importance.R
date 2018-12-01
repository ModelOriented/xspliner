library("breakDown")
library("randomForest")
library("DALEX")

# classification
data <- HR_data
data$left <- factor(data$left)
HR_rf_model <- randomForest(left~., data = data, ntree = 100)
importance(HR_rf_model)
table(predict(HR_rf_model, data))
explainer_rf  <- explain(HR_rf_model, data = data, y = data$left, predict_function = predict)
vd_rf <- variable_importance(explainer_rf, type = "raw",
                             loss_function = function(observed, predicted)
                               sum((observed != predicted)) / length(observed),
                             n_sample = 5000)
vd_rf
plot(vd_rf)

HR_glm_model <- glm(left~., data = data, family = "binomial")
explainer_glm <- explain(HR_glm_model, data = data, y = data$left)
logit <- function(x) exp(x)/(1+exp(x))
vd_glm <- variable_importance(explainer_glm, type = "raw",
                              loss_function = function(observed, predicted)
                                sum((observed != round(2 * logit(predicted)))) / length(observed),
                              n_sample = 5000)
vd_glm
plot(vd_glm)
plot(vd_rf, vd_glm)

# regression
data <- HR_data
#data$left <- factor(data$left)
HR_rf_model <- randomForest(factor(left)~., data = data, ntree = 100)
importance(HR_rf_model)
logit <- function(x) exp(x)/(1+exp(x))
#table(predict(HR_rf_model, data))
explainer_rf  <- explain(HR_rf_model, data = data, y = data$left,
                         predict_function = function(model, newdata, ...)
                           predict(model, newdata, type = "prob")[,2])
vd_rf <- variable_importance(explainer_rf, type = "difference",
                             loss_function = function(observed, predicted)
                               sum((observed != round(predicted))),
                             n_sample = 5000)
vd_rf
plot(vd_rf)
expl_rf_pdp  <- variable_response(explainer_rf, variable = "satisfaction_level", type = "pdp")#, which.class = 2, prob = TRUE)
plot(expl_rf_pdp)


HR_glm_model <- glm(left~., data = data, family = "binomial")
explainer_glm <- explain(HR_glm_model, data = data, y = data$left,
                         predict_function = function(model, newdata, ...) logit(predict(model, newdata, ...)))
vd_glm <- variable_importance(explainer_glm, type = "difference",
                              loss_function = function(observed, predicted)
                                sum((observed != round(2 * predicted))),
                              n_sample = 5000)
vd_glm
plot(vd_glm)

expl_glm_pdp  <- variable_response(explainer_glm, variable = "satisfaction_level", type = "pdp")#, which.class = 2, prob = TRUE)
plot(expl_glm_pdp)

plot(vd_rf, vd_glm)
plot(expl_rf_pdp, expl_glm_pdp)

