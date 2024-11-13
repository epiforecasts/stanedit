#' Extract the variables declared in a stan model
#'
#' @param x a \code{\link{stanedit}} object
#' @param block character (optional), the block or blocks in which to search;
#'   by default the whole model will be searched
#' @return a character vector of variable names
#' @export
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' get_vars(reg, "parameters")
get_vars <- function(x, block) {
  assert_class(x, "stanedit")
  if (missing(block)) {
    lines <- as.character(x)
  } else {
    lines <- get_block(x, block)
  }
  lines <- barebones(lines)
  declarations <- setdiff(
    grep(
      paste0(
        "^(array[[:space:]]+)?[a-z]+[[:space:]]+[^[:space:]]+[[:space:]]*$"),
      lines
    ),
    union(
      grep("^return[[:space:]]", lines),
      grep("[{}]", lines)
    )
  )
  vars <- sub("^.*[[:space:]]([^[:space:]]+)$", "\\1", lines[declarations])
  return(vars)
}

#' @title Get the contents of a block in a stan model
#'
#' @description
#' Returns the contents of a block in a stan model as a character vector of
#'   lines.
#' @return a character vector of the lines in the block
#' @param x a \code{\link{stanedit}} object
#' @param name name of the block
#' @param shell if TRUE (default:FALSE), will return the shell (i.e., the
#'   definition of the block) as well as content; this is useful, e.g., to see
#'   options passed to a \code{transition} or \code{ode} block
#' @importFrom checkmate assert_class
#' @export
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' get_block(reg, "parameters")
get_block <- function(x, name, shell = FALSE) {
  assert_class(x, "stanedit")
  if (missing(name)) {
    stop("The name of the block must be provided as 'name'")
  }
  block <- find_block(x, name)
  if (length(block) > 0) {
    lines <- as.character(x[block])
    if (!shell) {
      lines <- lines[-c(1, length(lines))]
    }
    return(lines)
  } else {
    return(character(0))
  }
}
