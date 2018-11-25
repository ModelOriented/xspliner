library(randomForest)
library(pdp)
data(boston)
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

# build xp_gam model with specified response method and approximation options
model_pdp <- xspline(
  cmedv ~
    xs(lstat,
       transform_opts = list(k = 6, increasing = FALSE),
       method_opts = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio,
       transform_opts = list(k = 4, increasing = FALSE),
       method_opts = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston
)

# plot pdp approximations
lstat <- pretty(boston$lstat, n = 50)
y <- environment(model_pdp)$xs(lstat)
plot(pdp::partial(boston.rf, "lstat"))
lines(lstat, y)

ptratio <- pretty(boston$ptratio, n = 50)
y <- environment(model_pdp)$xs(ptratio)
plot(pdp::partial(boston.rf, "ptratio"))
lines(ptratio, y)
