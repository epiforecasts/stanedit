% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove.R
\name{remove_lines}
\alias{remove_lines}
\title{Remove line(s) and/or block(s) in a libbi model}
\usage{
remove_lines(x, what)
}
\arguments{
\item{x}{a \code{\link{stanedit}} object}

\item{what}{either a vector of line number(s) to remove, or a vector of
blocks to remove (e.g., "parameters")}
}
\value{
the updated \code{stanedit} object
}
\description{
Removes one or more lines in a libbi model.
}
\examples{
model_file_name <- system.file(package = "stanedit", "regression.stan")
reg <- stanedit(filename = model_file_name)
reg <- remove_lines(reg, "model")
}
\seealso{
\code{\link{stanedit}}
}
