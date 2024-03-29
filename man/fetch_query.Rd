% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{fetch_query}
\alias{fetch_query}
\title{Send a query to API and return the result items as a tibble}
\usage{
fetch_query(url, take)
}
\arguments{
\item{url}{The full API URL specifying the endpoint and request parameters.}

\item{take}{The number of items to take from the API.}
}
\description{
\code{fetch_query} makes an API call to the given endpoint, converts the
results to a tibble, and cleans the column names.
}
\keyword{internal}
