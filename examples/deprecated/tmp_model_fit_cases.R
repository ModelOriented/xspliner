library(breakDown)
library(pdp)
library(randomForest)
library(xspliner)
library(DALEX)
library(ggplot2)

# randomForest
HR_rf_model <- randomForest(factor(left)~., data = breakDown::HR_data, ntree = 100)
explainer_rf  <- explain(HR_rf_model, data = HR_data, y = HR_data$left)

vd_rf <- variable_importance(explainer_rf, type = "raw")
plot(vd_rf)

## pdp
expl_rf_pdp  <- single_variable(explainer_rf, variable = "satisfaction_level", type = "pdp", which.class = 2, prob = TRUE)

gam_model <- xspline_approx(expl_rf_pdp, bs = "tp")
spline <- xspline_function(gam_model)
p <- plot(expl_rf_pdp)
xspline_plot(spline, add = TRUE, p)


# ROC:
inds <- createDataPartition(HR_data$left, p = 0.75)
HR_data_train <- HR_data[inds[[1]],]
HR_data_test  <- HR_data[-inds[[1]],]
HR_rf_model <- randomForest(left~., data = HR_data_train, ntree = 100)
library("plotROC")
pred_rf <- predict(HR_rf_model, HR_data_test)

roc.estimate <- calculate_roc(pred_rf, HR_data_test$left)
single.rocplot <- ggroc(roc.estimate)
plot_journal_roc(single.rocplot)

library("OptimalCutpoints")
pref_df <- data.frame(pred = pred_rf, truth = HR_data_test$left)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = "0")
summary(oc)
plot(oc, which=1)

## single variable model
simple_model <- glm(left ~ satisfaction_level, data = HR_data_train, family = binomial)
AIC(simple_model)
BIC(simple_model)

# logit_inv <- function(x) {
#   1 / (1 + exp(-x))
# }
# plot(HR_data$satisfaction_level, logit_inv(HR_data$left))
# plot(function(x) 1 / (1 + exp(-simple_model$coefficients[1] - x * simple_model$coefficients[2])), add = TRUE)
summary(simple_model)

library("plotROC")
pred_rf <- predict(simple_model, HR_data_test)

roc.estimate <- calculate_roc(pred_rf, HR_data_test$left)
single.rocplot <- ggroc(roc.estimate)
plot_journal_roc(single.rocplot)

library("OptimalCutpoints")
pref_df <- data.frame(pred = pred_rf, truth = HR_data_test$left)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = "0")
summary(oc)
plot(oc, which=1)

## model on pdp
model <- lm(left ~ spline(satisfaction_level), data = HR_data_train)
summary(model)
AIC(model)
BIC(model)

library("plotROC")
pred_rf <- predict(model, HR_data_test)

roc.estimate <- calculate_roc(pred_rf, HR_data_test$left)
single.rocplot <- ggroc(roc.estimate)
plot_journal_roc(single.rocplot)

library("OptimalCutpoints")
pref_df <- data.frame(pred = pred_rf, truth = HR_data_test$left)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = "0")
summary(oc)
plot(oc, which=1)

