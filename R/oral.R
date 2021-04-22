### Functions for downloading oral questions data

# Raw functions ---------------------------------------------------------------

query_oral <- function(url, take = NULL, warning = FALSE) {

    # Get raw data from API endpoint
    response <- httr::GET(url, timeout = 5000000)

    # Parse response text
    response_text <- httr::content(response, as = "text", encoding = "utf-8")

    # If server returned an error raise it with the response text
    if (response$status_code != 200) stop(query_error(response$status_code))

    # Convert response text to json
    response_json <- response_text %>%
        jsonlite::fromJSON(flatten = TRUE)

    if (warning == TRUE) {
        # Warn if the number of items available is greater than the maximum taken
        if ("Total" %in% names(response_json$PagingInfo)) {
            if (response_json$PagingInfo$Total > take) {
                warning(stringr::str_glue(
                    "The number of items available ({response_json$PagingInfo$Total}) ",
                    "is greater than the number of items taken."))
            }
        }
    }

    # Return
    response_json

}

#' Get the data items from an oral questions API response as a tibble
#'
#' \code{get_oral_response_items} converts the response returned from \code{query_oral}
#' to a tibble, and cleans the column names.
#'
#' @param response The response returned from a call to \code{query_oral}.
#' @keywords internal

get_oral_response_items <- function(response) {
    # Extract items as a tibble and clean names
    response$Response %>%
        tibble::as_tibble() %>%
        janitor::clean_names()
}

#' Send a query to oral API and return the result items as a tibble
#'
#' \code{fetch_oral_query} makes an API call to the given endpoint, converts the
#' results to a tibble, and cleans the column names.
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @param take The number of items to take from the API.
#' @keywords internal

fetch_oral_query <- function(url, take) {
    get_oral_response_items(query_oral(url, take, warning = FALSE))
}

#' Processes hidden oral pagination of results retrieved from the API
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @keywords internal

process_oral_pagination <- function(url, take) {

    if (take <= 25) {

        results <- fetch_oral_query(
            stringr::str_glue("{url}.take={take}"), take)

    } else {

        # Skip amount
        skip_amount <- seq(0, take - 25, 25)

        # Map pagination
        results <- purrr::map_df(skip_amount, function(amount) {
            fetch_oral_query(
                stringr::str_glue("{url}.skip={amount}"), take)
        })
    }
    results
}

#' Fetch data on oral questions as a tibble using a oral questions
#' endpoint URL
#'
#' \code{fetch_oral_from_url} fetches data on oral questions from an
#' endpoint URL and returns it as a tibble containing one row per question.
#' This internal function allows generic handling of requests for this data
#' to the oral questions endpoint with different URL parameters.
#'
#' @param url A valid URL requesting data from the oral questions endpoint.
#' @param summary A boolean indicating whether to exclude nested and empty
#'   columns in the results. The default is TRUE.
#' @param take The number of items to take from the API.
#' @keywords internal

fetch_oral_from_url <- function(url, summary = TRUE, take) {

    # Check if take is less than available
    query_oral(url, take, warning = TRUE)

    # Fetch the data
    oq <- process_oral_pagination(url, take)
    if (nrow(oq) == 0) return(oq)

    # Format
    oq <- format_oq_variable_names(oq)
    oq <- format_oq_variable_types(oq)

    if (summary == TRUE) {
        oq <- oq %>% dplyr::select(
            .data$question_id,
            .data$question_uin,
            .data$question_number,
            .data$question_date,
            .data$question_type,
            .data$question_status,
            .data$question_text,
            .data$question_member_mnis_id,
            .data$question_member_id,
            .data$question_member_pims_id,
            .data$question_member_name,
            .data$question_answer_date,
            .data$question_answer_body_id,
            .data$question_answer_body_name,
            .data$question_answer_member_id,
            .data$question_answer_minister_title,
            .data$question_answer_minister)
    }

    # Return
    oq
}
