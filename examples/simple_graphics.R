library(randomForest)
library(pdp)
data(boston)
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ rm + lstat + nox, data = boston)

# build xp_gam model with specified response method and approximation options
model_pdp <- xp_gam(
  cmedv ~ rm + lstat +
    xs(nox, method_opts = list(type = "pdp", grid.resolution = 100), spline_opts = list(k = 10, bs = "cr")),
  blackbox = boston.rf
)

xp_plot(model_pdp, "nox", data = boston, plot_data = TRUE)
xp_plot(model_pdp, "nox")
