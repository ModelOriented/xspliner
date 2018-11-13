library(magrittr)

## Test how formula can work on it's own environment (remove in the future)
xs_call <- list("x" = function(x) x ^ 2)
xf_call <- list("t" = function(t) sqrt(t))
enn <- new.env()
enn$xs_call <- xs_call
enn$xf_call <- xf_call
form_final <- as.formula("log(y) ~ xs_call[['x']](x) + xf_call[['t']](t)", env = enn)

df <- data.frame(y = 1:10, x = 101:110, z = 11:20, t = (0:9)^2, w = 10:1)
model.frame(form_final, data = df)
