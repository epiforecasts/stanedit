#' @name clean_model
#' @title Strip model code to its bare bones
#'
#' @description
#' Cleans the model by working out correct indents, removing long comments and
#'   merging lines (if desired)
#' @param x a \code{\link{stanedit}} object
#' @importFrom checkmate assert_character
#' @return the updated \code{stanedit} object
#' @seealso \code{\link{stanedit}}
#' @keywords internal
clean_model <- function(x) {
  assert_character(x)
  x <- as.character(x) ## strip stanedit class

  ## strip comments starting with //, ignoring any // inside string literals
  x <- sub("\"[^\"]*\"(*SKIP)(*FAIL)|//.*$", "", x, perl = TRUE)

  ## replace string literals with placeholders so that their contents are not
  ## altered by the comment removal, line merging and reformatting below
  strings <- character(0)
  for (i in seq_along(x)) {
    found <- regmatches(x[i], gregexpr("\"[^\"]*\"", x[i]))[[1]]
    for (s in found) {
      strings <- c(strings, s)
      x[i] <- sub(s, sprintf("\"@@STRING%d@@\"", length(strings)), x[i],
                  fixed = TRUE)
    }
  }

  ## remove long comments and merge lines
  i <- 1
  comment <- FALSE # flag to denote whether a line starts inside a comment
  while (i <= length(x)) {
    ## remove sections delimited by /* ... */ within lines
    x[i] <- gsub("/\\*[^(\\*)]*\\*/", "", x[i])
    if (comment) { ## we're inside a comment
      if (grepl("\\*/", x[i])) { ## comment ends but does not start on this line
        ## Remove everything before */ and unset the 'comment' flag so that in
        ## the next line we know that we're not in a comment any longer
        x[i] <- sub("^.*\\*/", "", x[i])
        comment <- FALSE
      } else { ## comment does not end on this line -- we remove the line
        x <- x[-i]
      }
    }

    ## if 'comment' is still true, we're inside the comment -- move on
    if (!comment) { ## we're not inside a comment
      if (grepl("/\\*", x[i])) {
        ## comment starts but does not end on this line.
        ## Remove everything after /* and set the 'comment' flag so that in the
        ## next line we know we're still inside the comment
        x[i] <- sub("/\\*.*$", "", x[i])
        comment <- TRUE
      }
      if (!grepl("[{};][[:space:]]*$", x[i]) && !(grepl("#", x[i])) &&
            i < length(x)) {
        ## line not finished -- merge lines
        x[i] <- paste(x[i], x[i + 1])
        x <- x[-(i + 1)]
      } else {
        i <- i + 1
      }
    }
  }

  ## remove multiple spaces
  x <- gsub("[[:space:]]+", " ", x)
  ## make sure there is a line break after opening braces
  x <- gsub("\\{", "{\n", x)
  ## make sure there is a line break before closing braces
  x <- gsub("\\}", "\n}", x)
  ## add newlines after semicolons
  x <- gsub(";", ";\n", x)
  ## split along newlines
  x <- unlist(strsplit(x, "\n"))
  ## remove trailing spaces
  x <- gsub("[[:space:]]*$", "", x)
  ## remove initial spaces
  x <- gsub("^[[:space:]]*", "", x)
  ## remove empty lines
  x <- x[x != ""]

  ## restore string literals
  for (k in seq_along(strings)) {
    x <- sub(sprintf("\"@@STRING%d@@\"", k), strings[k], x, fixed = TRUE)
  }

  model <- structure(x, class = "stanedit")

  check_model(model)

  return(model)
}
