#' @export
#' @rdname suppress_messages
recover_on_messages <- function(.expr, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler <- function(cond) {
    msg <- sprintf("message of class <%s>", paste(class(cond), collapse = "/"))
    message(msg)
    message(cond$message)
    recover()
  }
  with_condition(.expr, "message", handler, .f, ..., .classes = .classes, .packages = .packages)
}

#' @export
#' @rdname suppress_messages
recover_on_warnings <- function(.expr, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler <- function(cond) {
    msg <- sprintf("warning of class <%s>", paste(class(cond), collapse = "/"))
    message(msg)
    message(cond$message)
    recover()
  }
  with_condition(.expr, "warning", handler, .f, ..., .classes = .classes, .packages = .packages)
}

#' @export
#' @rdname suppress_messages
recover_on_errors <- function(.expr, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler <- function(cond) {
    msg <- sprintf("error of class <%s>", paste(class(cond), collapse = "/"))
    message(msg)
    message(cond$message)
    recover()
  }
  with_condition(.expr, "error", handler, .f, ..., .classes = .classes, .packages = .packages)
}
