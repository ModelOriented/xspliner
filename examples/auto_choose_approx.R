library(randomForest)
library(pdp)
data(boston)
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

model_pdp <- xspline(
  cmedv ~
    xs(lstat, transition = list(k = 6), effect = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transition = list(k = 4), effect = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston
)

# build xspline model with specified response method and approximation options,
# also choose automaticaly whether use bare or approximation
model_pdp_auto <- xspline(
  cmedv ~
    xs(lstat, transition = list(k = 6), effect = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transition = list(k = 4), effect = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston,
  alter = list(quantitative = 'consider_lm', quantitative = 'never')
)

summary(model_pdp)
summary(model_pdp_auto)
