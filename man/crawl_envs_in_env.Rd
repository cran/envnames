% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{crawl_envs_in_env}
\alias{crawl_envs_in_env}
\title{Crawl an environment in search of user environments}
\usage{
crawl_envs_in_env(envir)
}
\arguments{
\item{envir}{environment to search in.}
}
\value{
An array containing strings that represent the path to each environment found inside the
\code{envir} environment and any user environments nested within.
}
\description{
Crawl an environment in search of user environments
}
\keyword{internal}
