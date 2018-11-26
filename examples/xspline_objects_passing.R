# This example shows what possible objects can be passed into xspline function

library(randomForest)
library(pdp)
data(boston)
set.seed(101)

boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

# xspliner specific formula

model <- xspline(
  cmedv ~
    xs(lstat, transform_opts = list(k = 6), method_opts = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transform_opts = list(k = 4), method_opts = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston
)
summary(model)

# xspliner specific formula (can used just some predictors from the model one)

model <- xspline(
  cmedv ~ lstat + age,
  model = boston.rf,
  data = boston,
  exact = FALSE
)
summary(model)

# 'response ~ .' formula based on data

model <- xspline(
  cmedv ~ .,
  model = boston.rf
)
summary(model)

model <- xspline(
  cmedv ~ .,
  model = boston.rf,
  numeric_opts = list(method_opts = list(type = "pdp", grid.resolution = 40), transform_opts = list(k = 4)),
  alter = list(numeric = 'auto')
)
summary(model)

# predictive model

model <- xspline(
  boston.rf
)
summary(model)

# explainer object

explainer <- DALEX::explain(boston.rf, label = "boston")
model <- xspline(
  explainer
)
summary(model)
