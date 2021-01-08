### Functions for downloading written questions data

#' Fetch data on written questions as a tibble.
#'

fetch_questions_for_url <- function(url, summary = TRUE) {

    # Fetch the data
    wq <- query_results(url)
    if (nrow(wq) == 0) return(wq)



    # Convert date columns to Date
    wq <- wq %>% dplyr::mutate(
        dplyr::across(
            c(question_date,
              answer_date_expected,
              answer_date,
              answer_corrected_date,
              answer_holding_date),
            as.Date))


}
