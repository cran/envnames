% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{check_environment}
\alias{check_environment}
\title{Check whether a string corresponds to the name of an environment}
\usage{
check_environment(x, envir)
}
\arguments{
\item{x}{string to evaluate that may represent an environment}

\item{envir}{environment where \code{x} should be evaluated first. If it is not found there
it still searched for in the whole workspace.}
}
\value{
A list with two elements:
\itemize{
\item{\code{found}} whether the string contained in \code{x} is the name of an existing environment
in the workspace
\item{\code{env_name}} the name of the environment (after stripping out any system environments such
as globalenv(), baseenv() or a package environment) (for instance "globalenv()$env$env1" is returned
as "env$env1"), or NULL if no environment was found corresponding to the name given in \code{x}.
}
}
\description{
The input string is checked as a valid environment name. The environment can be a named environment
(system or package) or a user environment.
}
\keyword{internal}
