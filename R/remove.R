#' @title Remove line(s) and/or block(s) in a libbi model
#' @description
#' Removes one or more lines in a libbi model.
#'
#' @param x a \code{\link{stanedit}} object
#' @param what either a vector of line number(s) to remove, or a vector of
#'   blocks to remove (e.g., "parameters")
#' @return the updated \code{stanedit} object
#' @importFrom checkmate assert_class assert_character assert check_count
#'   check_character assert_logical
#' @seealso \code{\link{stanedit}}
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' reg <- remove_lines(reg, "model")
#' @export
remove_lines <- function(x, what) {
  assert_class(x, "stanedit")
  assert(
    check_count(what, positive = TRUE),
    check_character(what)
  )

  to_remove <- c()
  if (is.numeric(what)) {
    to_remove <- what
  } else {
    to_remove <- find_block(x, what)
  }

  if (length(to_remove) == 0) {
    warning("Nothing to remove.")
    return(x)
  } else {
    x <- x[-to_remove]
    return(clean_model(x))
  }

}
