library(randomForest)
library(pdp)
data(boston)
set.seed(101)

# build random forest model:
boston.rf <- randomForest(cmedv ~ rm + lstat + nox, data = boston)

# build xspliner model with specified response method and approximation options
model_pdp <- xspline(
  cmedv ~ rm + lstat +
    xs(nox, method_opts = list(type = "pdp", grid.resolution = 100), transform_opts = list(k = 10, bs = "cr")),
  model = boston.rf
)

plot(model_pdp, "nox", data = boston, plot_data = TRUE)
plot(model_pdp, "nox")
