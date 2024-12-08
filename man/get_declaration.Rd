% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_declaration}
\alias{get_declaration}
\title{Get the declaration of a variable in a stan model}
\usage{
get_declaration(x, name)
}
\arguments{
\item{x}{a \code{\link{stanedit}} object}

\item{name}{name of the variable}
}
\value{
a character vector of length 1 with the line where the variable is
declared, or an empty character vector if no declaration is found
}
\description{
Returns the contents of a block in a stan model as a character vector of
lines.
}
\examples{
model_file_name <- system.file(package = "stanedit", "regression.stan")
reg <- stanedit(filename = model_file_name)
get_declaration(reg, "alpha")
}
