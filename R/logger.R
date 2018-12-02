#' Helper function to print out log messages
#'
#' @param message Message that should be printed to R console
log_msg <- function(message) {
  if (getOption("xspliner.log", default = FALSE)) {
    cat(message)
  }
}
