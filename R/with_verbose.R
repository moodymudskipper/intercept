#' @export
#' @rdname suppress_messages
with_verbose_messages <- function(.expr, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler <- function(cond) {
    msg <- sprintf("message of class <%s>", paste(class(cond), collapse = "/"))
    message(msg)
    message(cond$message)
    print(rlang::trace_back())
  }
  with_condition(.expr, "message", handler, .f, ..., .classes = .classes, .packages = .packages)
}

#' @export
#' @rdname suppress_messages
with_verbose_warnings <- function(.expr, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler <- function(cond) {
    msg <- sprintf("warning of class <%s>", paste(class(cond), collapse = "/"))
    message(msg)
    message(cond$message)
    print(rlang::trace_back())
  }
  with_condition(.expr, "warning", handler, .f, ..., .classes = .classes, .packages = .packages)
}
