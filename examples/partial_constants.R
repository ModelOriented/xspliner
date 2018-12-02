library(DALEX)
library(caret)
library(gam)
library(xspliner)
data(apartments)

set.seed(123)
variable <- "construction.year"
regr_rf <- train(m2.price~., data = apartments, method="rf", ntree = 100)
model_xs <- xspline(regr_rf, data = apartments, bare = c("floor", "no.rooms"),
                    xs_opts = list(transition = list(bs = "ps", fx = FALSE, k = 20, m = -1)))
plot(model_xs, "surface")

