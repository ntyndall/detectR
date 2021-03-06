% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_from_list.R
\name{max_from_list}
\alias{max_from_list}
\title{Get Max Token From List}
\usage{
max_from_list(tokenVec)
}
\arguments{
\item{tokenVec}{A vector of tokens plus scores.}
}
\value{
A vector of numerical values representing the tokenzied
 scores.
}
\description{
A function that wraps around \code{max_token} to
 vectorize the token scores.
}
