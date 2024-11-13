##' Remove everything between opening/closing characters, including nesting
##'
##' Useful for e.g. removing bounds, array indices etc.
##' @param x a character vector
##' @param opening opening character
##' @param closing closing character
##' @return stanedit object with the bits removed
##' @importFrom checkmate assert_class assert_character
##' @keywords internal
remove_contained <- function(x, opening, closing) {
  assert_character(x)
  assert_character(opening)
  assert_character(closing)
  ## filter out lines that contain the opening character(s)
  line_ids <- grep(paste0("[", opening, "]"), x)
  for (line_id in line_ids) {
    chars <- strsplit(x[line_id], split = "")[[1]]
    paren_level <- 0
    result <- ""
    for (char in chars) {
      if (char == opening) {
        paren_level <- paren_level + 1
      } else if (char == closing && paren_level > 0) {
        paren_level <- paren_level - 1
      } else if (paren_level == 0) {
        result <- paste0(result, char)
      }
    }
    x[line_id] <- result
  }
  return(x)
}

#' @title Strip model code to its bare bones
#' @keywords internal
#' @param x a character vector
#' @return a character vector of the model code
barebones <- function(x) {
  assert_character(x)
  ## strip out unneeded characters
  x <- remove_contained(x, "[", "]")
  x <- remove_contained(x, "(", ")")
  x <- remove_contained(x, "<", ">")
  ## remove assignments
  x <- sub("[[:space:]]*[*-/+]?[=~].*$", "", x)
  ## remove trailing semicolons
  x <- sub(";$", "", x)
  ## remove trailing whitespace
  x <- sub("[[:space:]]*$", "", x)
  return(x)
}
