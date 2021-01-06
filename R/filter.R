### Functions for filtering dates

#' Filter a tibble of data based on the given from and to dates.
#'
#' \code{filter_dates} takes a tibble which contains data on a time bound
#' activity and returns the subset of rows where that activity took place
#' within a given period. The tibble must contain two columns of Date objects,
#' which record the start and end dates of an activity. The from and to dates
#' provided are used to find all rows where some part of the period
#' of activity took place within the period of filtering. The filtering
#' process is inclusive: as long as at least one day of activity falls within
#' the filtering period, the row is returned.
#'
#' @param df A tibble containing data on a time bound activity.
#' @param start_col The name of the column that contains the start date for
#'   the activity.
#' @param end_col The name of the column that contains the end date for the
#'   activity.
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis
#'   of the to_date.
#' @return  A tibble with the same structure as the input df containing
#'   the rows that meet the filtering criteria.
#' @keywords internal

filter_dates <- function(
    df,
    start_col,
    end_col,
    from_date = NULL,
    to_date = NULL) {

    # Check the start and end columns exist
    if (! start_col %in% colnames(df)) {
        stop(missing_column_error(start_col))
    }

    if (! end_col %in% colnames(df)) {
        stop(missing_column_error(end_col))
    }

    # Check the dataframe has rows
    if (nrow(df) == 0) return(df)

    # Check there are dates to filter
    if (is.null(from_date) && is.null(to_date)) return(df)

    # Handle from and to dates
    from_date <- handle_date(from_date)
    to_date <- handle_date(to_date)

    # Check from date is before to date
    if ((!is.null(from_date)) && (!is.null(to_date)) && (from_date > to_date)) {
        stop("to_date is before from_date")
    }

    # Set default values
    from_after_end <- FALSE
    to_before_start <- FALSE

    # Get matching rows
    if (!is.null(from_date)) {
        from_after_end <- purrr::map_lgl(df[[end_col]], function(d) {
            ifelse(is.null(d), FALSE, from_date > d)
        })
    }

    if (!is.null(to_date)) {
        to_before_start <- purrr::map_lgl(df[[start_col]], function(d) {
            ifelse(is.null(d), FALSE, to_date < d)
        })
    }

    df[!(from_after_end | to_before_start), ]
}

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

#' Cast a numeric value to a Date
#'
#' @keywords internal

cast_date <- function(date_num) {
    tryCatch(
        as.Date(date_num, origin = "1970-01-01"),
        error = function(e) stop("Could not cast numeric to date"))
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
