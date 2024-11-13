#' @title Remove line(s) and/or block(s) in a libbi model
#' @description
#' Removes one or more lines in a libbi model.
#'
#' @param x a \code{\link{stanedit}} object
#' @param what either a vector of line number(s) to remove, or a vector of
#'   blocks to remove (e.g., "parameters")
#' @param only only remove lines assigning given names (as a vector of character
#'   strings)
#' @param type which types of lines to remove, either "all", "sample" (i.e.,
#'   lines with a "~") or "assignment" (lines with a "=") (default:
#'   "all")
#' @param preserve_shell if TRUE (default: FALSE), preserve the definition of a
#'   block even if all lines are removed; this is useful to preserve options
#'   passed to a \code{transition} or \code{ode} block
#' @return the updated \code{stanedit} object
#' @importFrom checkmate assert_class
#' @seealso \code{\link{stanedit}}
#' @examples
#' model_file_name <- system.file(package = "stanedit", "regression.stan")
#' reg <- stanedit(filename = model_file_name)
#' reg <- remove_lines(reg, "model", only = "y", type = "sample")
#' @export
remove_lines <- function(x, what, only, type = c("all", "assignment", "sample"),
                         preserve_shell = FALSE) {
  assert_class(x, "stanedit")
  if (missing(what)) {
    stop("'what' must be given")
  }
  type <- match.arg(type)
  to_remove <- c()
  if (is.numeric(what)) {
    to_remove <- what
  } else if (is.character(what)) {
    to_remove <- find_block(x, what, inner = preserve_shell)
  } else {
    stop("'what' must be a numeric or character vector.")
  }

  operators <- list(assignment = "=", sample = "~")

  ## check if we don't want to remove everything
  if (length(to_remove) > 0 && (type != "all" || !missing(only))) {
    if (type == "all") {
      op_types <- unlist(operators)
    } else {
      op_types <- operators[[type]]
    }
    pattern <- paste0(
      "^([A-Za-z_0-9[\\]][[:space:]A-Za-z_0-9,[\\]]*)",
      "(", paste(op_types, collapse = "|"), ")"
    )
    assign_lines <- grep(pattern, x[to_remove], perl = TRUE)
    assign_vars <- sub(paste0(pattern, ".*$"), "\\1",
      x[to_remove][assign_lines],
      perl = TRUE
    )
    assign_vars <- sub("[[:space:]]", "", sub("\\[.*]", "", assign_vars))
    if (!missing(only)) {
      assign_lines <- assign_lines[assign_vars %in% only]
    }
    if (is.character(what) && !preserve_shell &&
      length(assign_lines) == length(to_remove) - 2) {
      assign_lines <- c(1, assign_lines, length(to_remove))
    }
    to_remove <- to_remove[assign_lines]
  }

  if (length(to_remove) > 0) {
    x <- x[-to_remove]
  }

  return(clean_model(x))
}
