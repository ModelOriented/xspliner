#' Predict xspliner method
#'
#' @param xspliner Object of class 'xspliner'.
#' @param newdata Data that should be prediciton based on.
#'
#' @export
predict.xspliner <- function(xspliner, newdata) {
  mgcv::predict.gam(xspliner, newdata = newdata)
}
