library(breakDown)
library(pdp)
library(randomForest)
library(xspliner)
library(DALEX)
library(ggplot2)

# randomForest
HR_rf_model <- randomForest(factor(left)~., data = breakDown::HR_data, ntree = 100)
explainer_rf  <- explain(HR_rf_model, data = HR_data,
                         predict_function = function(model, x) predict(model, x, type = "prob")[,2])

## pdp
expl_rf_pdp  <- single_variable(explainer_rf, variable = "satisfaction_level", type = "pdp", which.class = 2, prob = TRUE)

gam_model <- xspline_approx_gam(expl_rf_pdp)
rms_model <- xspline_approx_rms(expl_rf_pdp)
spline_gam <- xspline_function_gam(gam_model)
spline_rms <- xspline_function_rms(rms_model)
p <- plot(expl_rf_pdp)
xspline_plot(spline_gam, add = TRUE, p)
xspline_plot(spline_rms, add = TRUE, p)

## ale
expl_rf_ale  <- single_variable(explainer_rf, variable = "satisfaction_level", type = "ale", which.class = 2, prob = TRUE)

gam_model <- xspline_approx_gam(expl_rf_ale)
rms_model <- xspline_approx_rms(expl_rf_ale)
spline_gam <- xspline_function_gam(gam_model)
spline_rms <- xspline_function_rms(rms_model)
p <- plot(expl_rf_ale)
xspline_plot(spline_gam, add = TRUE, p)
xspline_plot(spline_rms, add = TRUE, p)

# glm
logit <- function(x) exp(x)/(1+exp(x))

HR_glm_model <- glm(left~., data = breakDown::HR_data, family = "binomial")
explainer_glm <- explain(HR_glm_model, data = HR_data, trans=logit)

## pdp
expl_glm_pdp <- single_variable(explainer_glm, "satisfaction_level", "pdp")

gam_model <- xspline_approx_gam(expl_glm_pdp)
rms_model <- xspline_approx_rms(expl_glm_pdp)
spline_gam <- xspline_function_gam(gam_model)
spline_rms <- xspline_function_rms(rms_model)
p <- plot(expl_glm_pdp)
xspline_plot(spline_gam, add = TRUE, p)
xspline_plot(spline_rms, add = TRUE, p)

## ale
expl_glm_ale <- single_variable(explainer_glm, "satisfaction_level", "ale")

gam_model <- xspline_approx_gam(expl_glm_ale)
rms_model <- xspline_approx_rms(expl_glm_ale)
spline_gam <- xspline_function_gam(gam_model)
spline_rms <- xspline_function_rms(rms_model)
p <- plot(expl_glm_ale)
xspline_plot(spline_gam, add = TRUE, p)
xspline_plot(spline_rms, add = TRUE, p)

#xgboost
library(xgboost)
library(xspliner)
model_matrix_train <- model.matrix(left ~ . -1, HR_data)
data_train <- xgb.DMatrix(model_matrix_train, label = HR_data$left)
param <- list(max_depth = 2, objective = "binary:logistic")

HR_xgb_model <- xgb.train(param, data_train, nrounds = 50)
explainer_xgb <- explain(HR_xgb_model, data = model_matrix_train)

## pdp
expl_xgb_pdp <- single_variable(explainer_xgb, "satisfaction_level", "pdp")

gam_model <- xspline_approx_gam(expl_xgb_pdp)
rms_model <- xspline_approx_rms(expl_xgb_pdp)
spline_gam <- xspline_function_gam(gam_model)
spline_rms <- xspline_function_rms(rms_model)
p <- plot(expl_xgb_pdp)
xspline_plot(spline_gam, add = TRUE, p)
xspline_plot(spline_rms, add = TRUE, p)

## ale
expl_xgb_ale <- single_variable(explainer_xgb, "satisfaction_level", "ale")

gam_model <- xspline_approx_gam(expl_xgb_ale)
rms_model <- xspline_approx_rms(expl_xgb_ale)
spline_gam <- xspline_function_gam(gam_model)
spline_rms <- xspline_function_rms(rms_model)
p <- plot(expl_xgb_ale)
xspline_plot(spline_gam, add = TRUE, p)
xspline_plot(spline_rms, add = TRUE, p)
