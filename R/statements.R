### Functions for downloading written statements data

# Raw functions ---------------------------------------------------------------

#' Fetch data on written statements as a tibble using a written statements
#' endpoint URL
#'
#' \code{fetch_statements_from_url} fetches data on written statements from an
#' endpoint URL and returns it as a tibble containing one row per statement.
#' This internal function allows generic handling of requests for this data
#' to the written statements endpoint with different URL parameters.
#'
#' @param url A valid URL requesting data from the written statements endpoint.
#' @param summary A boolean indicating whether to exclude nested and empty
#'   columns in the results. The default is TRUE.
#' @param take The number of items to take from the API.
#' @keywords internal

fetch_statements_from_url <- function(url, summary = TRUE, take) {

    # Check if take is less than available
    query(url, take, warning = TRUE)

    # Fetch the data
    ws <- process_pagination(url, take)
    if (nrow(ws) == 0) return(ws)

    # Format
    ws <- format_ws_variable_names(ws)
    ws <- format_ws_variable_types(ws)

    if (summary == TRUE) {
        ws <- ws %>% dplyr::select(
            .data$statement_id,
            .data$statement_uin,
            .data$statement_date,
            .data$statement_house,
            .data$statement_notice_number,
            .data$statement_title,
            .data$statement_text,
            .data$statement_body_id,
            .data$statement_body_name,
            .data$statement_member_mnis_id,
            .data$statement_member_name,
            .data$statement_member_role,
            .data$statement_member_party,
            .data$statement_member_constituency)
    }

    # Return
    ws
}

#' Processes hidden pagination of results retrieved from the API
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @keywords internal

process_pagination <- function(url, take) {

    if (take <= 20) {

        results <- fetch_query(
            stringr::str_glue("{url}&take={take}"), take)

    } else {

        # Skip amount
        skip_amount <- seq(0, take - 20, 20)

        # Map pagination
        results <- purrr::map_df(skip_amount, function(amount) {
            fetch_query(
                stringr::str_glue("{url}&skip={amount}"), take)
        })
    }
    results
}

# Main functions --------------------------------------------------------------

#' Fetch data on written statements by statement date
#'
#' \code{fetch_written_statements} fetches data on written statements and
#' returns it as a tibble containing one row per statement arranged by
#' statement date.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the on_date.
#' @param house A string indicating either the House of Commons or House of
#'   Lords. Possible values include "c", "C", "Commons", "l", "L", "Lords".
#'   The default value is NULL, which means results from both Houses are returned.
#' @param take An integer indicating the number of records to take from
#'   the API. By default the most recent 1,000 records are taken.
#' @param summary A boolean indicating whether to exclude nested and empty
#'   columns in the results. The default is TRUE.
#' @export

fetch_written_statements <- function(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE) {

    # Set house if set
    house <- set_house(house)

    # Check date format
    from_date <- handle_date(from_date)
    to_date <- handle_date(to_date)
    on_date <- handle_date(on_date)

    # Check from date is before to date
    if ((!is.null(from_date)) && (!is.null(to_date)) && (from_date > to_date)) {
        stop("to_date is before from_date")
    }

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WS_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&madeWhenFrom={from_date}",
            "&madeWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WS_BASE_URL,
            "&take={take}",
            "&house={house}"))
    }

    # Return
    fetch_statements_from_url(url, summary, take) %>%
        dplyr::arrange(.data$statement_date)
}

#' Fetch data on written statements by body and statement date
#'
#' \code{fetch_written_statements_body} fetches data on written statements
#' and returns it as a tibble containing one row per statement
#' arranged by statement date.
#'
#' @param body_id An integer representing the body responsible for the written
#'   statement.
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the on_date.
#' @param house A string indicating either the House of Commons or House of
#'   Lords. Possible values include "c", "C", "Commons", "l", "L", "Lords".
#'   The default value is NULL, which means results from both Houses are returned.
#' @param take An integer indicating the number of records to take from
#'   the API. By default the most recent 1,000 records are taken.
#' @param summary A boolean indicating whether to exclude nested and empty
#'   columns in the results. The default is TRUE.
#' @export

fetch_written_statements_body <- function(
    body_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE) {

    # Check department ID provided
    if (is.null(body_id)) stop(missing_argument("body_id"))
    if (length(body_id) > 1) stop(multiple_ids("body"))

    # Set house if set
    house <- set_house(house)

    # Check date format
    from_date <- handle_date(from_date)
    to_date <- handle_date(to_date)
    on_date <- handle_date(on_date)

    # Check from date is before to date
    if ((!is.null(from_date)) && (!is.null(to_date)) && (from_date > to_date)) {
        stop("to_date is before from_date")
    }

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WS_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answeringBodies={body_id}",
            "&madeWhenFrom={from_date}",
            "&madeWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WS_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answeringBodies={body_id}"))
    }

    # Return
    fetch_statements_from_url(url, summary, take) %>%
        dplyr::arrange(.data$statement_date)
}

#' Fetch data on written statements by Member and statement date
#'
#' \code{fetch_written_statements_member} fetches data on written statements
#' and returns it as a tibble containing one row per statement
#' arranged by statement date.
#'
#' @param member_mnis_id An integer representing the MNIS ID for Commons and
#'   Lords members.
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the on_date.
#' @param house A string indicating either the House of Commons or House of
#'   Lords. Possible values include "c", "C", "Commons", "l", "L", "Lords".
#'   The default value is NULL, which means results from both Houses are returned.
#' @param take An integer indicating the number of records to take from
#'   the API. By default the most recent 1,000 records are taken.
#' @param summary A boolean indicating whether to exclude nested and empty
#'   columns in the results. The default is TRUE.
#' @export

fetch_written_statements_member <- function(
    member_mnis_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE) {

    # Check member ID provided
    if (is.null(member_mnis_id)) stop(missing_argument("member_mnis_id"))
    if (length(member_mnis_id) > 1) stop(multiple_ids("member"))

    # Set house if set
    house <- set_house(house)

    # Check date format
    from_date <- handle_date(from_date)
    to_date <- handle_date(to_date)
    on_date <- handle_date(on_date)

    # Check from date is before to date
    if ((!is.null(from_date)) && (!is.null(to_date)) && (from_date > to_date)) {
        stop("to_date is before from_date")
    }

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WS_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&members={member_mnis_id}",
            "&madeWhenFrom={from_date}",
            "&madeWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WS_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&members={member_mnis_id}"))
    }

    # Return
    fetch_statements_from_url(url, summary, take) %>%
        dplyr::arrange(.data$statement_date)
}

