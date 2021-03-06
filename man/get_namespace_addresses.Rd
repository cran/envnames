% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{get_namespace_addresses}
\alias{get_namespace_addresses}
\title{Return the memory address of namespace environments in the search() list}
\usage{
get_namespace_addresses()
}
\value{
Array containing the namespace addresses as values and the package names as names (in the form
of e.g. "package:base")
}
\description{
This function returns the memory address of all the namespace environments of the packages found
in the \code{search()} path.
}
\keyword{internal}
