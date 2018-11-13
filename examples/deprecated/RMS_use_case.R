library(breakDown)
library(pdp)
library(randomForest)
library(xspliner)
library(DALEX)
library(ggplot2)
library(rms)

# randomForest
data <- HR_data
data$left <- factor(data$left)
data$promotion_last_5years <- factor(data$promotion_last_5years)
data$Work_accident <- factor(data$Work_accident)
HR_rf_model <- randomForest(left~., data = data, ntree = 100)
explainer_rf  <- explain(HR_rf_model, data = data,
                         predict_function = function(model, x) predict(model, x, type = "prob")[,2],
                         y = data$left)

## pdp
expl_rf_pdp  <- single_variable(explainer_rf, variable = "satisfaction_level", type = "pdp", which.class = 2, prob = TRUE)

gam_model <- xspline_approx_gam(expl_rf_pdp)
dd <- datadist(expl_rf_pdp[, c("x", "y")])
options(datadist="dd")
options(ols_knots_number = 30)
rms_model <- xspline_approx_rms(expl_rf_pdp)
spline_gam <- xspline_function_gam(gam_model)
spline_rms <- xspline_function_rms(rms_model)
p <- plot(expl_rf_pdp)
p2 <- xspline_plot(spline_gam, add = TRUE, p)
xspline_plot(spline_rms, add = TRUE, p2)

#
vi_rf <- variable_importance(explainer_rf)
plot(vi_rf)
