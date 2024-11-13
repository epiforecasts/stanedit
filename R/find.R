#' @title Find a block in a stan model
#'
#' @description
#' Finds a block and returns the range of line numbers encompassed by that
#'   block.
#' @return an integerr vector, the range of line numbers
#' @seealso \code{\link{stanedit}}
#' @keywords internal
#' @param x a \code{\link{stanedit}} object
#' @param name of the block to find
#' @param inner only return the inner part of the block (not the block
#'   definition)
#' @importFrom checkmate assert_class
#' @export
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' find_block(reg, "parameters")
find_block <- function(x, name, inner = FALSE) {
  assert_class(x, "stanedit")
  lines <- as.character(x)
  sub_regexp <- paste0(
    "^[[:space:]]*(sub[[:space:]]+)?[[:space:]]*", name,
    "([[:space:]][a-zA-Z0-9_\\.]+)?[[:space:]]*(\\(.*\\))?[[:space:]]*\\{"
  )
  sub_line <- grep(sub_regexp, lines)
  if (length(sub_line) == 1) {
    lines[sub_line] <- sub(sub_regexp, "", lines[sub_line])
    open_braces <- 1
    line <- sub_line - 1
    while (open_braces > 0) {
      line <- line + 1
      open_braces <- open_braces +
        nchar(gsub("\\}", "", lines[line])) -
        nchar(gsub("\\{", "", lines[line]))
    }
    if (inner) {
      return((sub_line + 1):(line - 1))
    }
    return(sub_line:line)
  } else {
    return(integer(0))
  }
}

#' @title Find the declaration of a variable in a stan model
#'
#' @description
#' Finds a block and returns the range of line numbers encompassed by that
#'   block.
#' @return an integer, the line numbes where the variable is declared; an empty
#'   integer if the declaration is not found
#' @seealso \code{\link{stanedit}}
#' @keywords internal
#' @param x a \code{\link{stanedit}} object
#' @param name of the block to find
#' @importFrom checkmate assert_class
#' @export
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' find_declaration(reg, "alpha")
find_declaration <- function(x, name) {
  assert_class(x, "stanedit")
  lines <- barebones(as.character(x))

  ## remove leading whitespace
  declaration <- setdiff(
    grep(
      paste0(
        "^(array[[:space:]]+)?[a-z]+[[:space:]]+",
        name,
        "[[:space:]]*$"),
      lines
    ),
    union(
      grep("^return[[:space:]]", lines),
      grep("[{}]", lines)
    )
  )
  return(declaration)
}
