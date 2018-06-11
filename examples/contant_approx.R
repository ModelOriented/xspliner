library(DALEX)
library(caret)
library(gam)
library(xspliner)
data(apartments)
data(apartmentsTest)

set.seed(123)
variable <- "construction.year"
regr_rf <- train(m2.price~., data = apartments, method="rf", ntree = 100)

explainer_regr_rf <- DALEX::explain(regr_rf, label="rf",
                                    data = apartmentsTest, y = apartmentsTest$m2.price)

vi_regr_rf <- variable_importance(explainer_regr_rf, loss_function = loss_root_mean_square)
plot(vi_regr_rf)


pdp_regr_rf  <- variable_response(explainer_regr_rf, variable =  variable, type = "pdp")
plot(pdp_regr_rf)

spline_on_pdp <- xspline_approx(pdp_regr_rf, bs = "ps", fx = FALSE, k = 20, m = -1)
spline <- xspline_function(spline_on_pdp)
p <- plot(pdp_regr_rf)
xspline_plot(spline, add = TRUE, p)
