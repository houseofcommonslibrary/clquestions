% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/questions.R
\name{fetch_written_questions_body}
\alias{fetch_written_questions_body}
\title{Fetch data on written questions and answers by body and date tabled}
\usage{
fetch_written_questions_body(
  body_id = NULL,
  from_date = NULL,
  to_date = NULL,
  on_date = NULL,
  house = NULL,
  take = 1000,
  summary = TRUE
)
}
\arguments{
\item{body_id}{An integer representing the body responsible for answering
the written question.}

\item{from_date}{A string or Date representing a date. If a string is used
it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
default value is NULL, which means no records are excluded on the basis of
the from_date.}

\item{to_date}{A string or Date representing a date. If a string is used
it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
default value is NULL, which means no records are excluded on the basis of
the to_date.}

\item{on_date}{A string or Date representing a date. If a string is used
it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
default value is NULL, which means no records are excluded on the basis of
the on_date.}

\item{house}{A string indicating either the House of Commons or House of
Lords. Possible values include "c", "C", "Commons", "l", "L", "Lords".
The default value is NULL, which means results from both Houses are returned.}

\item{take}{An integer indicating the number of records to take from
the API. By default the most recent 1,000 records are taken.}

\item{summary}{A boolean indicating whether to exclude nested and empty
columns in the results. The default is TRUE.}
}
\description{
\code{fetch_written_questions_body} fetches data on written questions
and answers and returns it as a tibble containing one row per question/answer
arranged by question date.
}
