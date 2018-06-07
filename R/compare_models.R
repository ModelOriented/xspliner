library(DALEX)
library(caret)
library(gam)
data(apartments)
data(apartmentsTest)

set.seed(123)
variable <- "surface"
regr_rf <- train(m2.price~., data = apartments, method="rf", ntree = 100)

explainer_regr_rf <- DALEX::explain(regr_rf, label="rf",
                                    data = apartmentsTest, y = apartmentsTest$m2.price)

vi_regr_rf <- variable_importance(explainer_regr_rf, loss_function = loss_root_mean_square)
# plot(vi_regr_rf)
pdp_regr_rf  <- variable_response(explainer_regr_rf, variable =  variable, type = "pdp")
# plot(pdp_regr_rf)

spline_on_pdp <- xspline_approx(pdp_regr_rf, bs = "tp")
spline <- xspline_function(spline_on_pdp)
# p <- plot(pdp_regr_rf)
# xspline_plot(spline, add = TRUE, p)

model_on_pdp <- lm(data = apartments, m2.price ~ spline(construction.year))
model_on_data <- mgcv::gam(m2.price ~ s(construction.year), data = apartments, qr = TRUE)

gam_on_pdp <- DALEX::explain(model_on_pdp, label="gam_on_pdp",
                      data = apartmentsTest, y = apartmentsTest$m2.price)
gam_on_data <- DALEX::explain(model_on_data, label="gam_on_data",
                      data = apartmentsTest, y = apartmentsTest$m2.price,
                      predict_function = function(m,x) mgcv::predict.gam(m,x,type = "response"))

perf_pdp <- model_performance(gam_on_pdp)

perf_data <- model_performance(gam_on_data)
plot(perf_pdp, perf_data)

