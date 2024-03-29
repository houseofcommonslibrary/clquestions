% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{get_response_items}
\alias{get_response_items}
\title{Get the data items from an API response as a tibble}
\usage{
get_response_items(response)
}
\arguments{
\item{response}{The response returned from a call to \code{query}.}
}
\description{
\code{get_response_items} converts the response returned from \code{query}
to a tibble, and cleans the column names.
}
\keyword{internal}
