library(randomForest)
library(pdp)
data(boston)
boston_raw <- boston
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

# build xp_gam model with specified response method and approximation options
model <- xp_gam(
  cmedv ~
    xs(lstat, spline_opts = list(k = 6), method_opts = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, spline_opts = list(k = 4), method_opts = list(type = "pdp", grid.resolution = 40)) +
    age,
  blackbox = boston.rf,
  data = boston
)

# check model summary
summary(model)

# compare with standard gam model
summary(mgcv::gam(cmedv ~ s(lstat, k = 6) + s(ptratio, k = 4) + age, data = boston))
