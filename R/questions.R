### Functions for downloading written questions data

#' Fetch data on written questions as a tibble.
#'

fetch_questions_for_url <- function(url, summary = TRUE) {

    # Fetch the data
    wq <- query_results(url)
    if (nrow(wq) == 0) return(wq)

    # Rename the ids to be informative
    wq <- wq %>% dplyr::rename(
        question_answer_id = .data$value_uin,
        question_member_mnis_id = .data$value_asking_member_id,
        answer_body_id = .data$value_answering_body_id,
        answer_member_mnis_id = .data$value_answering_member_id,
        answer_correcting_member_mnis_id = .data$value_correcting_member_id)

    # Rename date variables
    wq <- wq %>% dplyr::rename(
        question_date = .data$value_date_tabled,
        answer_date_expected = .data$value_date_for_answer,
        answer_date = .data$value_date_answered,
        answer_corrected_date = .data$value_date_answer_corrected,
        answer_holding_date = .data$value_date_holding_answer)

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
