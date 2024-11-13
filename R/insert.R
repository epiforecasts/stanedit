#' @title Insert lines in a stan model
#' @description
#' Inserts one or more lines into a libbi model. If one of \code{before} or
#'   \code{after} is given, the line(s) will be inserted before or after a given
#'   line number or block name, respectively. If one of \code{at_beginning of}
#'   or \code{at_end_of} is given, the lines will be inserted at the
#'   beginning/end of the block, respectively.
#'
#' @param x a \code{\link{stanedit}} object
#' @param lines vector or line(s)
#' @param before line number before which to insert line(s)
#' @param after line number after which to insert line(s)
#' @param at_beginning_of block at the beginning of which to insert lines(s)
#' @param at_end_of block at the end of which to insert lines(s)
#' @return the updated \code{stanedit} object
#' @importFrom checkmate assert_class
#' @seealso \code{\link{stanedit}}
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' insert_lines(reg, lines = "alpha ~ std_normal()", after = 12)
#' @export
insert_lines <- function(x, lines, before, after, at_beginning_of, at_end_of) {
  assert_class(x, "stanedit")
  args <- match.call()
  arg_name <- setdiff(names(args), c("", "x", "lines"))
  if (length(arg_name) != 1) {
    stop(
      "insert_lines needs exactly three arguments, 'x', 'lines' and one of ",
      "'before', 'after', 'at_beginning_of' or 'at_end_of'"
    )
  }
  arg <- eval.parent(args[[arg_name]])
  if (is.numeric(arg)) arg <- as.integer(arg)

  if (arg_name %in% c("before", "after") && is.integer(arg)) {
    if (arg_name == "before") {
      after <- before - 1
    }
    if (after > length(x)) {
      stop("model only has ", length(x), " lines, higher requested.")
    }
  } else {
    block_lines <- find_block(x, arg)
    if (length(block_lines) == 0) {
      x <- add_block(x, arg)
      block_lines <- find_block(x, arg)
    }
    if (arg_name == "before") {
      after <- block_lines[1] - 1
    } else if (arg_name == "after") {
      after <- block_lines[length(block_lines)]
    } else if (arg_name == "at_beginning_of") {
      after <- block_lines[1]
    } else if (arg_name == "at_end_of") {
      after <- block_lines[length(block_lines)] - 1
    } else {
      stop("Unknown argument: ", arg_name)
    }
  }

  if (after == 0) {
    x <- c(lines, x)
  } else if (after == length(x)) {
    x <- c(x[1:after], lines)
  } else {
    x <- c(x[1:after], lines, x[(after + 1):length(x)])
  }
  return(clean_model(x))
}
