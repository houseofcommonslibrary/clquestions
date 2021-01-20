### Functions for filtering dates

#' Take a date which may be a string or a date and returns a date.
#'
#' \code{handle_date} takes a date which may be a Date or an ISO 8601 date
#' string, checks it is valid, and returns the date as a Date. NULL values are
#' returned unmodified. This function raises an error if it is unable to
#' handle the date.
#'
#' @keywords internal

handle_date <- function(d) {
    if (is.null(d)) {
        return(d)
    } else if (class(d) == "Date") {
        return(d)
    } else if(class(d) == "character") {
        return(parse_date(d))
    } else {
        stop(date_format_error(d))
    }
}

#' Parse an ISO 8601 date from a string
#'
#' @keywords internal

parse_date <- function(date_str) {

    valid_pattern <- "^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])$"
    if (! grepl(valid_pattern, date_str)) stop(date_format_error(date_str))

    tryCatch(
        as.Date(date_str, origin = "1970-01-01"),
        error = function(e) stop(date_format_error(date_str)))
}
