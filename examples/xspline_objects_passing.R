# This example shows what possible objects can be passed into xspline function

library(randomForest)
library(pdp)
data(boston)
set.seed(101)

boston.rf <- randomForest(cmedv ~ lstat + ptratio + age, data = boston)

# xspliner specific formula

model <- xspline(
  cmedv ~
    xs(lstat, transition = list(k = 6), effect = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transition = list(k = 4), effect = list(type = "pdp", grid.resolution = 40)) +
    age,
  model = boston.rf,
  data = boston
)
summary(model)

# when xs has no parameters default one are taken

model <- xspline(
  cmedv ~
    xs(lstat) +
    xs(ptratio, transition = list(k = 4), effect = list(type = "pdp", grid.resolution = 40)) +
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
  consider = "all"
)
summary(model)

# we can consider whereter to use linear or xs transformation automatically
model <- xspline(
  cmedv ~ lstat + ptratio + age,
  model = boston.rf,
  data = boston,
  consider = "all",
  xs_opts = list(transition = list(alter = "auto"))
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
  xs_opts = list(
    effect = list(type = "pdp", grid.resolution = 40),
    transition = list(k = 4, alter = "auto"))
)
summary(model)

model <- xspline(
  cmedv ~ .,
  model = boston.rf,
  xs_opts = list(
    effect = list(type = "pdp"),
    transition = list(alter = "auto"))
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

# factor predictor
set.seed(101)

boston.rf <- randomForest(cmedv ~ lstat + ptratio + chas + age, data = boston)

model <- xspline(
  cmedv ~
    xs(lstat, transition = list(k = 6), effect = list(type = "pdp", grid.resolution = 60)) +
    xs(ptratio, transition = list(k = 4), effect = list(type = "pdp", grid.resolution = 40)) +
    xf(chas) +
    age,
  model = boston.rf,
  data = boston,
  xf_opts = list(transition = list(alter = "always"))
)
summary(model)
