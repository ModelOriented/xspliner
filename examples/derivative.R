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

xs <- transition(model, "ptratio", "function")
range <- attributes(xs)$variable_range
n_points <- 50
x <- seq(range[1], range[2], length.out = n_points)
y <- xs(x)
eps <- (range[2] - range[1]) / n_points / 10
y2 <- xs(x + eps)
y1 <- xs(x)
dy <- (y2 - y1) / eps
plot(x, y, type = "l")
par(new = T)
plot(x, dy, type = "l", axes=F, xlab=NA, ylab=NA)
axis(side = 4)
