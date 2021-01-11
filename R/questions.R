### Functions for downloading written questions data

#' Fetch data on written questions as a tibble using a written questions
#' endpoint URL
#'
#' \code{fetch_questions_from_url} fetches data on written questions from an
#' endpoint URL and returns it as a tibble containing one row per question.
#' This internal function allows generic handling of requests for this data
#' to the written questions endpoint with different URL parameters.
#'
#' @param url A valid URL requesting data from the written questions endpoint.
#' @param summary A boolean indicating whether to exclude nested, empty and
#'   duplicated columns. Variables expected to have high NA rates will also
#'   be excluded. The default is TRUE.
#' @keywords internal

fetch_questions_from_url <- function(url, summary = TRUE) {

    # Fetch the data
    wq <- query_results(url)
    if (nrow(wq) == 0) return(wq)

    # Format
    wq <- format_variable_names(wq)
    wq <- format_variable_types(wq)

    # Summary if TRUE
    if (summary == TRUE) {
        wq <- wq %>%
            dplyr::select(
                .data$question_answer_id,
                .data$question_answer_uin,
                .data$question_answer_subject,
                .data$question_house,
                .data$question_date,
                .data$question_text,
                .data$question_is_named_day,
                .data$question_is_withdrawn,
                .data$question_member_mnis_id,
                .data$question_member_name,
                .data$question_member_party,
                .data$question_member_constituency,
                .data$answer_date_expected,
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

#' Fetch data on written questions
#'
#' \code{fetch_written_questions} fetches data on written questions and
#' returns it as a tibble containing one row per question.
#'
#' By default this function returns a subset of the columns and ignores any
#' nested and redundant columns. Set \code{summary = FALSE} when calling the
#' function to retrieve the full data.
#'
#' @param summary A boolean indicating whether to exclude nested, empty and
#'   duplicated columns. Variables expected to have high NA rates will also
#'   be excluded. The default is TRUE.
#' @param take An integer indicating the number of questions to take from
#'   the API. By default the newest 1,000 questions are taken.
#' @export

fetch_written_questions <- function(
    question_from_date = NULL,
    question_to_date = NULL,
    question_on_date = NULL,
    answer_from_date = NULL,
    answer_to_date = NULL,
    answer_on_date = NULL,
    summary = TRUE,
    take = 1000) {

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
            "&Take={take}",
            "&tabledWhenFrom={from_date}",
            "&tableWhen"))
    }



    # Fetch data for all written questions
    url <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&Take={take}"))

    fetch_questions_from_url(url, summary = summary)
}

#' Fetch data on Commons written questions
#'
#' \code{fetch_commons_wqs} fetches data on written questions from
#' the House of Commons and returns it as a tibble containing one
#' row per question.
#'
#' By default this function returns a subset of the columns and ignores any
#' nested and redundant columns. Set \code{summary = FALSE} when calling the
#' function to retrieve the full data.
#'
#' @param summary A boolean indicating whether to exclude nested, empty and
#'   duplicated columns. Variables expected to have high NA rates will also
#'   be excluded. The default is TRUE.
#' @param take An integer indicating the number of questions to take from
#'   the API. The default is 1,000.
#' @export

fetch_commons_wqs <- function(summary = TRUE, take = 1000) {

    # Fetch data for all written questions
    url <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&Take={take}"))

    fetch_questions_from_url(url, summary = summary)
}
