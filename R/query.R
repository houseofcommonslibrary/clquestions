### Functions for querying API data and processing data

#' Send a query to API and return the response as a named list
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @param take The number of items to take from the API.
#' @keywords internal

fetch_query <- function(url, take) {

    # Get raw data from API endpoint
    response <- httr::GET(url)

    # Parse response text
    response_text <- httr::content(response, as = "text", encoding = "utf-8")

    # If server returned an error raise it with the response text
    if (response$status_code != 200) stop(query_error(response$status_code))

    # Convert response text to json
    response_json <- response_text %>%
        jsonlite::fromJSON(flatten = TRUE)

    # Warn if the number of items available is greater than the maximum taken
    if ("totalResults" %in% names(response_json)) {
        if (response_json$totalResults > take) {
            warning(stringr::str_glue(
                "The number of items available ({response_json$totalResults}) ",
                "is greater than the maximum number of items taken."))
        }
    }

    # Return
    response_json
}

#' Get the data items from an API response as a tibble
#'
#' @param response The response returned from a call to \code{fetch_query}.
#' @keywords internal

get_response_items <- function(response) {
    # Extract items as a tibble and clean names
    response$results %>%
        tibble::as_tibble() %>%
        janitor::clean_names()
}

#' Send a query to API and return the result items as a tibble
#'
#' \code{query_results} makes an API call to the given endpoint, converts the
#' results to a tibble, and cleans the column names.
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @param take The number of items to take from the API.
#' @keywords internal

query_results <- function(url, take) {
    get_response_items(fetch_query(url, take))
}
