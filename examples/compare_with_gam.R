library(randomForest)
library(pdp)
data(boston)
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

# build xspliner model with specified response method and approximation options
model_pdp <- xspline(
  cmedv ~
    xs(lstat, transform_opts = list(k = 6), method_opts = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transform_opts = list(k = 4), method_opts = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston
)

model_ale <- xspline(
  cmedv ~
    xs(lstat, transform_opts = list(k = 6), method_opts = list(type = "ale", K = 60)) +
    xs(ptratio, transform_opts = list(k = 4), method_opts = list(type = "ale", K = 40)) +
    age,
  model = boston.rf,
  data = boston
)

# check model summary
summary(model_pdp)

summary(model_ale)

# compare with standard gam model
summary(mgcv::gam(cmedv ~ s(lstat, k = 6) + s(ptratio, k = 4) + age, data = boston))

