#' @title Print the lines of a stan model
#'
#' @description
#' Prints all lines in a stan model
#' @param x a \code{\link{stanedit}} object
#' @param spaces number of spaces for indentation
#' @param screen whether to print to screen (default: TRUE). In that case, line
#'   numbers will be added; otherwise, a character vector will be returned.
#' @param ... ignored
#' @rdname print
#' @name print.stanedit
#' @return if \code{screen} is \code{FALSE}, a character vector of model lines
#' @keywords internal
#' @export
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' print(reg, spaces = 4)
print.stanedit <- function(x, spaces = 2, screen = interactive(), ...) {
  x <- clean_model(x)
  if (length(x) == 0) {
    if (screen) cat("// empty", "\n")
  } else {
    vec <- c()
    indent <- 0
    for (i in seq_along(x)) {
      if (grepl("\\}", x[i])) indent <- indent - 1
      indent_spaces <- paste(rep(" ", max(0, indent * spaces)), collapse = "")
      vec <- c(vec, paste0(indent_spaces, x[i]))
      if (grepl("\\{", x[i])) {
        indent <- indent + 1
      }
    }
    if (screen) {
      line_num_indent <- nchar(as.character(length(vec)))
      line_nums <- vapply(seq_along(vec), function(y) {
        paste0(
          c(rep(" ", line_num_indent - nchar(as.character(y))), y),
          collapse = ""
        )
      }, character(1))
      cat(paste(paste(line_nums, vec, sep = ": "), collapse = "\n"), sep = "\n")
    } else {
      return(vec)
    }
  }
}
