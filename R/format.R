### Functions for formatting columns

#' Rename variable names
#'
#' \code{format_variable_names} takes a tibble returned from \code{query_results}
#' and renames variables with meaningful descriptions.
#'
#' @param results The tibble returned from \code{query_results}.
#' @keywords internal

format_variable_names <- function(results) {

    results <- results %>% dplyr::rename(
        question_links = .data$links,
        question_answer_id = .data$value_id,
        question_member_mnis_id = .data$value_asking_member_id,
        question_house = .data$value_house,
        question_member_has_interest = .data$value_member_has_interest,
        question_date = .data$value_date_tabled,
        answer_date_expected = .data$value_date_for_answer,
        question_answer_uin = .data$value_uin,
        question_text = .data$value_question_text,
        answer_body_id = .data$value_answering_body_id,
        answer_body_name = .data$value_answering_body_name,
        question_is_withdrawn = .data$value_is_withdrawn,
        question_is_named_day = .data$value_is_named_day,
        question_is_grouped = .data$value_grouped_questions,
        answer_is_holding = .data$value_answer_is_holding,
        answer_is_correction = .data$value_answer_is_correction,
        answer_member_mnis_id = .data$value_answering_member_id,
        answer_correcting_member_mnis_id = .data$value_correcting_member_id,
        answer_date = .data$value_date_answered,
        answer_text = .data$value_answer_text,
        answer_text_original = .data$value_original_answer_text,
        answer_text_comparable = .data$value_comparable_answer_text,
        answer_date_corrected = .data$value_date_answer_corrected,
        answer_date_holding = .data$value_date_holding_answer,
        answer_attachment_count = .data$value_attachment_count,
        answer_attachments = .data$value_attachments,
        question_answer_subject = .data$value_heading,
        question_member_mnis_id_2 = .data$value_asking_member_id_2,
        question_member_list_as = .data$value_asking_member_list_as,
        question_member_name = .data$value_asking_member_name,
        question_member_party = .data$value_asking_member_party,
        question_member_party_colour = .data$value_asking_member_party_colour,
        question_member_party_abbreviation = .data$value_asking_member_party_abbreviation,
        question_member_constituency = .data$value_asking_member_member_from,
        question_member_thumbnail_url = .data$value_asking_member_thumbnail_url,
        answer_member_mnis_id_2 = .data$value_answering_member_id_2,
        answer_member_list_as = .data$value_answering_member_list_as,
        answer_member_name = .data$value_answering_member_name,
        answer_member_party = .data$value_answering_member_party,
        answer_member_party_colour = .data$value_answering_member_party_colour,
        answer_member_party_abbreviation = .data$value_answering_member_party_abbreviation,
        answer_member_constituency = .data$value_answering_member_member_from,
        answer_member_thumbnail_url = .data$value_answering_member_thumbnail_url)

    if (sum(results$answer_is_correction) != 0) {

        results <- results %>% dplyr::rename(
            answer_correcting_member_mnis_id_2 = .data$value_correcting_member_id_2,
            answer_correcting_member_list_as = .data$value_correcting_member_list_as,
            answer_correcting_member_name = .data$value_correcting_member_name,
            answer_correcting_member_party = .data$value_correcting_member_party,
            answer_correcting_member_party_colour = .data$value_correcting_member_party_colour,
            answer_correcting_member_party_abbreviation = .data$value_correcting_member_party_abbreviation,
            answer_correcting_member_constituency = .data$value_correcting_member_member_from,
            answer_correcting_member_thumbnail_url = .data$value_correcting_member_thumbnail_url)

    }

    results
}

#' Format variable type
#'
#' \code{format_variable_type} takes a tibble returned from \code{query_results}
#' and specifies variable data types.
#'
#' @param results The tibble returned from \code{query_results}.
#' @keywords internal

format_variable_types <- function(results) {

    # Character
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(question_answer_id,
              question_member_mnis_id,
              question_house,
              question_answer_uin,
              question_text,
              answer_body_id,
              answer_body_name,
              answer_member_mnis_id,
              answer_correcting_member_mnis_id,
              answer_text,
              answer_text_original,
              answer_text_comparable,
              question_answer_subject,
              question_member_mnis_id_2,
              question_member_list_as,
              question_member_name,
              question_member_party,
              question_member_party_colour,
              question_member_party_abbreviation,
              question_member_constituency,
              question_member_thumbnail_url,
              answer_member_mnis_id_2,
              answer_member_list_as,
              answer_member_name,
              answer_member_party,
              answer_member_party_colour,
              answer_member_party_abbreviation,
              answer_member_constituency,
              answer_member_thumbnail_url),
            as.character))

    if (sum(results$answer_is_correction) != 0) {

        results <- results %>% dplyr::mutate(
            dplyr::across(
                c(answer_correcting_member_mnis_id_2,
                  answer_correcting_member_list_as,
                  answer_correcting_member_name,
                  answer_correcting_member_party,
                  answer_correcting_member_party_colour,
                  answer_correcting_member_party_abbreviation,
                  answer_correcting_member_constituency,
                  answer_correcting_member_thumbnail_url),
                as.character))
    }

    # List
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(question_links,
              answer_attachments),
            as.list))

    # Logical
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(question_member_has_interest,
              question_is_withdrawn,
              question_is_named_day,
              answer_is_holding,
              answer_is_correction),
            as.logical))

    # Numeric
    results$answer_attachment_count <- as.integer(results$answer_attachment_count)

    # Empty string -> NA
    results$question_is_grouped[lengths(results$question_is_grouped) == 0] <- NA_character_
    results[results == ""] <- NA


    # As Date
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(question_date,
              answer_date_expected,
              answer_date,
              answer_date_corrected,
              answer_date_holding),
            as.Date))

    # Return
    results
}

