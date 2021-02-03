### Functions for downloading written questions data

# Raw functions ---------------------------------------------------------------

#' Fetch data on written questions as a tibble using a written questions
#' endpoint URL
#'
#' \code{fetch_questions_from_url} fetches data on written questions from an
#' endpoint URL and returns it as a tibble containing one row per question.
#' This internal function allows generic handling of requests for this data
#' to the written questions endpoint with different URL parameters.
#'
#' @param url A valid URL requesting data from the written questions endpoint.
#' @param summary A boolean indicating whether to exclude nested and empty
#'   columns in the results. The default is TRUE.
#' @param take The number of items to take from the API.
#' @keywords internal

fetch_questions_from_url <- function(url, summary = TRUE, take) {

    # Check if take is less than available
    query(url, take, warning = TRUE)

    # Fetch the data
    wq <- fetch_query(url, take)
    if (nrow(wq) == 0) return(wq)

    # Format
    wq <- format_wq_variable_names(wq)
    wq <- format_wq_variable_types(wq)

    if (summary == TRUE) {
        wq <- wq %>% dplyr::mutate(question_is_answered = dplyr:::if_else(
            is.na(.data$answer_date) == TRUE, FALSE, TRUE)) %>%
            dplyr::select(
                .data$question_answer_id,
                .data$question_answer_uin,
                .data$question_answer_subject,
                .data$question_answer_house,
                .data$question_date,
                .data$question_text,
                .data$question_is_named_day,
                .data$question_is_withdrawn,
                .data$question_is_answered,
                .data$question_member_has_interest,
                .data$question_member_mnis_id,
                .data$question_member_name,
                .data$question_member_party,
                .data$question_member_constituency,
                .data$answer_date,
                .data$answer_text,
                .data$answer_is_holding,
                .data$answer_is_correction,
                .data$answer_body_id,
                .data$answer_body_name,
                .data$answer_member_mnis_id,
                .data$answer_member_name,
                .data$answer_member_party,
                .data$answer_member_constituency)
    }

    # Return
    wq
}

# Main functions --------------------------------------------------------------

#' Fetch data on written questions and answers by date tabled
#'
#' \code{fetch_written_questions} fetches data on written questions and answers
#' returns it as a tibble containing one row per question/answer arranged by
#' question date.
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

fetch_written_questions <- function(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE) {

    # Set house if set
    house <- set_house(house)

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&tabledWhenFrom={from_date}",
            "&tabledWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answered=Any"))
    }

    # Return
    fetch_questions_from_url(url, summary, take) %>%
        dplyr::arrange(.data$question_date)
}

#' Fetch data on written questions and answers by date answered
#'
#' \code{fetch_written_answers} fetches data on written questions and answers and
#' returns it as a tibble containing one row per question/answer arranged by
#' answer date.
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
#'  The default value is NULL, which means results from both Houses are returned.
#' @param take An integer indicating the number of records to take from
#'   the API. By default the most recent 1,000 records are taken.
#' @param summary A boolean indicating whether to exclude nested and empty
#'   columns in the results. The default is TRUE.
#' @export

fetch_written_answers <- function(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE) {

    # Set house if set
    house <- set_house(house)

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answeredWhenFrom={from_date}",
            "&answeredWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answered=Answered"))
    }

    # Return
    fetch_questions_from_url(url, summary, take) %>%
        dplyr::arrange(.data$answer_date)
}

#' Fetch data on written questions and answers by body and date tabled
#'
#' \code{fetch_written_questions_body} fetches data on written questions
#' and answers and returns it as a tibble containing one row per question/answer
#' arranged by question date.
#'
#' @param body_id An integer representing the body responsible for answering
#'   the written question.
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

fetch_written_questions_body <- function(
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

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answeringBodies={body_id}",
            "&tabledWhenFrom={from_date}",
            "&tabledWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answeringBodies={body_id}",
            "&answered=Any"))
    }

    # Return
    fetch_questions_from_url(url, summary, take) %>%
        dplyr::arrange(.data$question_date)
}

#' Fetch data on written questions and answers by body and date answered
#'
#' \code{fetch_written_answers_body} fetches data on written questions
#' and answers and returns it as a tibble containing one row per question/answer
#' arranged by answer date.
#'
#' @param body_id An integer representing the body responsible for answering
#'   the written question.
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

fetch_written_answers_body <- function(
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

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answeringBodies={body_id}",
            "&answeredWhenFrom={from_date}",
            "&answeredWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&answeringBodies={body_id}",
            "&answered=Answered"))
    }

    # Return
    fetch_questions_from_url(url, summary, take) %>%
        dplyr::arrange(.data$answer_date)
}


#' Fetch data on written questions and answers by Member and date tabled
#'
#' \code{fetch_written_questions_member} fetches data on written questions
#' and answers and returns it as a tibble containing one row per question/answer
#' arranged by question date.
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

fetch_written_questions_member <- function(
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

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&askingMemberId={member_mnis_id}",
            "&tabledWhenFrom={from_date}",
            "&tabledWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "&askingMemberId={member_mnis_id}",
            "&answered=Any"))
    }

    # Return
    fetch_questions_from_url(url, summary, take) %>%
        dplyr::arrange(.data$question_date)
}

#' Fetch data on written questions and answers by Member and date answered
#'
#' \code{fetch_written_answers_member} fetches data on written questions
#' and answers and returns it as a tibble containing one row per question/answer
#' arranged by answer date.
#'
#' @param member_mnis_id An integer representing the Government body
#'   responsible for answering the written question.
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

fetch_written_answers_member <- function(
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

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "answeringMemberId={member_mnis_id}",
            "&answeredWhenFrom={from_date}",
            "&answeredWhenTo={to_date}"))

    } else {

        # Fetch data for all written questions
        url <- stringr::str_glue(stringr::str_c(
            WQ_BASE_URL,
            "&take={take}",
            "&house={house}",
            "answeringMemberId={member_mnis_id}",
            "&answered=Answered"))
    }

    # Return
    fetch_questions_from_url(url, summary, take) %>%
        dplyr::arrange(.data$answer_date)
}









