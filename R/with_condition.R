with_condition <- function(.expr, condition, handler, .f = NULL, ..., .classes = NULL, .packages = NULL) {
  handler0 <- function(cond) {
    msg <- cli::ansi_strip(conditionMessage(cond))
    body_qualifies <-
      is.null(.f) ||
      if (is.character(.f)) grepl(.f, msg) else rlang::as_function(.f)(msg, ...)
    class_qualifies <-
      is.null(.classes) || inherits(cond, .classes)
    package_qualifies <-
      is.null(.packages) ||
      any(setdiff(sapply(sys.frames(), function(e) environmentName(topenv(e))), c("base", "intercept")) %in% .packages)
    if (body_qualifies && class_qualifies && package_qualifies) {
      handler(cond)
    }
  }

  if (condition == "message") {
    withCallingHandlers( .expr, message = handler0)
  } else {
    withCallingHandlers( .expr, warning = handler0)
  }
}
