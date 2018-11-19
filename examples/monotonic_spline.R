library(randomForest)
library(pdp)
data(boston)
boston_raw <- boston
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston_raw)

# build xp_gam model with specified response method and approximation options
model_pdp <- xp_gam(
  cmedv ~
    xs(lstat,
       spline_opts = list(k = 6, increasing = FALSE),
       method_opts = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio,
       spline_opts = list(k = 4, increasing = FALSE),
       method_opts = list(type = "pdp", grid.resolution = 40)) +
    age,
  blackbox = boston.rf,
  data = boston_raw
)

# plot pdp approximations
lstat <- pretty(boston_raw$lstat, n = 50)
y <- environment(model_pdp)$xs(lstat)
plot(pdp::partial(boston.rf, "lstat"))
lines(lstat, y)

ptratio <- pretty(boston_raw$ptratio, n = 50)
y <- environment(model_pdp)$xs(ptratio)
plot(pdp::partial(boston.rf, "ptratio"))
lines(ptratio, y)
