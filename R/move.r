#' Move stan code elsewere
#'
#' @param x a \code{\link{stanedit}} object
#' @param lines vector of line numbers to move
#' @param ... arguments passed to \code{\link{insert_lines}}
#' @return a character vector of variable names
#' @export
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' move(reg, find_declaration(reg, "alpha"), at_end_of = "data")
move <- function(x, lines, ...) {
  assert_class(x, "stanedit")
  lines_content <- as.character(x)[lines]
  x <- remove_lines(x, lines)
  x <- insert_lines(x, lines_content, ...)
  return(clean_model(x))
}
