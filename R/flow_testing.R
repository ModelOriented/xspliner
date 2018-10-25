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
          xs(age, spline_opts = list(k = 2), method_opts = list(type = "pdp", grid.resolution = 20)),
  blackbox = boston.rf,
  data = boston
)
