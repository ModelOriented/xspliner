library(randomForest)
library(pdp)
data(boston)  # load the boston housing data
boston = boston
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)
# oko <- partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE)


#debug(transformed_formula_object)
model <- xp_gam(
  cmedv ~ xs(lstat, spline_opts = list(k = 6), method_opts = list(type = "pdp", grid.resolution = 60)) +
          xs(ptratio, spline_opts = list(k = 4), method_opts = list(type = "pdp", grid.resolution = 40)) +
          age,
  blackbox = boston.rf,
  data = boston
)

summary(model)
summary(mgcv::gam(cmedv ~ s(lstat, k = 6) + s(ptratio, k = 4) + age, data = boston))

var <- "age"
pred_var <- pretty(range(boston[[var]]), 100)
plot(model[["xs_env"]][[var]][["blackbox_response_obj"]])
lines(pred_var, predict(model[["xs_env"]][[var]][["blackbox_response_approx"]], newdata = data.frame(pred_var = pred_var)))
