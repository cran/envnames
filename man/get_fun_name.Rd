% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fun_name.r
\name{get_fun_name}
\alias{get_fun_name}
\title{Return the name of the current function or a calling function in the chain}
\usage{
get_fun_name(n = 0)
}
\arguments{
\item{n}{number of levels to go up in the calling chain in search of the calling function name.
Defaults to \code{n=0}, meaning that the name returned is the name of the function that calls
\code{get_fun_name}.}
}
\value{
A string containing the name of the function that has been called \code{n} levels up
from the function calling \code{get_env_name}. The function name is returned without context,
that is the enclosing environment of the function is not part of the returned value.
(e.g. if the function is \code{env1$f} or \code{env1$env2$f} only \code{"f"} will be returned).
}
\description{
Return the name of the function that has been called \code{n} levels up from a given function's body.
This function is intended to be called only within a function.
}
\examples{
# Show the name of the active function
f <- function() { cat("We are in function:", get_fun_name(), "\n") }
f()

# Show the name of the calling function
f <- function(x) { cat("Calling function name is:", get_fun_name(1), "\n") }
env1 <- new.env()
with(env1, g <- function() { f(3) })
env1$g()
}
\seealso{
\link{get_fun_calling} to retrieve the name of the function with its context (e.g. \code{"env1$f"}).
}
