#' @title Writes a bi model to a file.
#' @description
#' Writes a bi model to a file given by \code{filename}. The extension '.bi'
#'   will be added if necessary.
#'
#' @param x a \code{\link{stanedit}} object
#' @param filename name of the file to be written
#' @return the return value of the \code{\link{writeLines}} call.
#' @seealso \code{\link{stanedit}}
#' @importFrom checkmate assert_class
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' new_file_name <- tempfile("reg", fileext = ".stan")
#' write_model(reg, new_file_name)
#' @export
write_model <- function(x, filename) {
  assert_class(x, "stanedit")
  if (!grepl("\\.stan$", filename)) {
    filename <- paste(filename, "bi", sep = ".")
  }

  writeLines(print(x, screen = FALSE), con = filename, sep = "\n")
}
