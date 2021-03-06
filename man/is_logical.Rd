% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{is_logical}
\alias{is_logical}
\title{Check whether an object contains a valid logical value}
\usage{
is_logical(x)
}
\arguments{
\item{x}{object to check. A scalar is assumed.}
}
\value{
boolean indicating whether the scalar object contains a valid logical value (i.e. TRUE or FALSE)
and is not NA nor NULL, and has positive length.
FALSE is returned if the object does not exist.
}
\description{
Check whether an object contains a valid logical value
}
\details{
This function silently handles special cases for which is.null() and is.na() (called by this function)
may return a warning, such as functions objects or environments.
(e.g. the warning "Warning message: In is.na(g) : is.na() applied to non-(list or vector) of type 'closure')"
}
\keyword{internal}
