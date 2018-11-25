library(randomForest)
library(pdp)
data(boston)
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

model_pdp <- xspline(
  cmedv ~
    xs(lstat, transform_opts = list(k = 6), method_opts = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transform_opts = list(k = 4), method_opts = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston
)

# build xspline model with specified response method and approximation options,
# also choose automaticaly whether use bare or approximation
model_pdp_auto <- xspline(
  cmedv ~
    xs(lstat, transform_opts = list(k = 6), method_opts = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transform_opts = list(k = 4), method_opts = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston,
  alter = list(numeric = 'auto', factor = 'never')
)

summary(model_pdp)
summary(model_pdp_auto)
