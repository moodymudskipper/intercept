#' Intercept messages and warnings
#'
#' @param .expr expression to evaluate
#' @param .f predicate function to apply on the message, a purrr lambda, or a
#' string containing a regular expression
#' @param ... additional parameters to forward to `.f`
#' @param .classes condition classes to consider, by default everything qualifies
#' @param .packages packages to consider, by default everything qualifies
#'
#' @return The output of `.expr`
#' @export
#'
#' @examples
#' suppress_warnings(sqrt(-1), "foo")
#' suppress_warnings(sqrt(-1), "NaN")
suppress_messages <- function(.expr, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler <- function(cond) {
    invokeRestart("muffleMessage")
  }
  with_condition(.expr, "message", handler, .f, ..., .classes = .classes, .packages = .packages)
}

#' @export
#' @rdname suppress_messages
suppress_warnings <- function(.expr, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler <- function(cond) {
    invokeRestart("muffleWarning")
  }
  with_condition(.expr, "warning", handler, .f, ..., .classes = .classes, .packages = .packages)
}
