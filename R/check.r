#' @name check model
#' @title Check model and produce messages/warnings if any problems
#'
#' @description
#' This function currently only checks for balanced braces.
#' @param x a \code{\link{stanedit}} object
#' @importFrom checkmate assert_class
#' @return invisible \code{x} (called for it's side effects)
#' @seealso \code{\link{stanedit}}
#' @keywords internal
check_model <- function(x) {
  x <- assert_class(x, "stanedit")
  ## check
  opening_curls <- length(grep("\\{", x))
  closing_curls <- length(grep("\\}", x))
  if (opening_curls != closing_curls) {
    warning("Model contains unbalanced braces.")
  }
  return(invisible(x))
}
