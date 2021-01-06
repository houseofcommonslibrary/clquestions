### Functions for generating error messages

#' Report an error with a http response
#'
#' @param code The status code of the server response
#' @keywords internal

query_error <- function(code) {
    stringr::str_glue(
        "The server responded with the following status code: {code}")
}
