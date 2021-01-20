### Functions for generating error messages

#' Report an error with a http response
#'
#' @param code The status code of the server response
#' @keywords internal

query_error <- function(code) {
    stringr::str_glue(
        "The server responded with the following status code: {code}")
}

#' Report an error parsing a date string
#'
#' @param date_str The date string that could not be parsed.
#' @keywords internal

date_format_error <- function(date_str) {
    stringr::str_glue(stringr::str_c(
        "{date_str} is not a valid Date or ",
        "date string: use format \"YYYY-MM-DD\""))
}

#' Report a missing argument
#'
#' @param argument The name of the missing argument.
#' @keywords internal

missing_argument <- function(argument) {
    stringr::str_glue("A {argument} has not been supplied.")
}

#' Report multiple IDs
#'
#' @keywords internal

multiple_ids <- function(x) {
    stringr::str_glue(
        "Multiple {x} IDs have been supplied. Only one ID is allowed.")
}

#' Report multiple search terms
#'
#' @keywords internal

multiple_terms <- function() {
    stringr::str_glue(
        "Multiple search terms have been supplied. Only one term is allowed.")
}
