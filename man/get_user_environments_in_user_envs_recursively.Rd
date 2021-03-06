% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{get_user_environments_in_user_envs_recursively}
\alias{get_user_environments_in_user_envs_recursively}
\title{Get the user environments defined recursively within the given user environments}
\usage{
get_user_environments_in_user_envs_recursively(env_user_names, envir)
}
\arguments{
\item{env_user_names}{names of the user environments where the recursive search is carried out.}

\item{envir}{user environment where ALL environments in \code{env_user_names} live,
or \code{NULL} if those environments may live in different environments. In this case
the names of the environment where each \code{env_user_names} live is taken
from its names attribute.}
}
\value{
a list containing the following elements:
\itemize{
\item{fullnames} the full names of the environments
\item{addresses} the addresses of the environments
\item{locationaddresses} the addresses of the locations of the environments
}
}
\description{
Get the user environments defined recursively within the given user environments
}
\keyword{internal}
