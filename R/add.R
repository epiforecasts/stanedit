#' @name add_block
#' @title Add a block to a stan model
#'
#' @description
#' Add a block to a stan model. If that block exists, it will be removed first.
#' @return a \code{\link{stanedit}} object containing the new block
#' @param x a \code{\link{stanedit}} object
#' @param name name of the block
#' @param lines character vector, lines in the block
#' @importFrom checkmate assert_class
#' @export
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' reg <- add_block(
#'   reg,
#'   "transformed parameters",
#'   c("real gamma;", "gamma = alpha * beta;")
#' )
add_block <- function(x, name, lines) {
  assert_class(x, "stanedit")
  x <- remove_lines(x, what = name)
  if (missing(lines)) {
    lines <- c()
  }
  x <- c(
    x,
    paste(name, "{"),
    paste(lines, sep = "\n"), "}"
  )
  return(clean_model(x))
}
