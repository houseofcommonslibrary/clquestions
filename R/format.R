### Functions for formatting columns and function arguments

# Written questions -----------------------------------------------------------

#' Rename written question variable names
#'
#' \code{format_wq_variable_names} takes a tibble returned from \code{fetch_query}
#' and renames variables with meaningful descriptions.
#'
#' @param results The tibble returned from \code{fetch_query}.
#' @keywords internal

format_wq_variable_names <- function(results) {

    results <- results %>% dplyr::rename(
        question_links = .data$links,
        question_answer_id = .data$value_id,
        question_member_mnis_id = .data$value_asking_member_id,
        question_answer_house = .data$value_house,
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
        question_member_thumbnail_url = .data$value_asking_member_thumbnail_url)

    if (all(is.na(results$answer_member_mnis_id)) == FALSE) {
        results <- results %>% dplyr::rename(
            answer_member_mnis_id_2 = .data$value_answering_member_id_2,
            answer_member_list_as = .data$value_answering_member_list_as,
            answer_member_name = .data$value_answering_member_name,
            answer_member_party = .data$value_answering_member_party,
            answer_member_party_colour = .data$value_answering_member_party_colour,
            answer_member_party_abbreviation = .data$value_answering_member_party_abbreviation,
            answer_member_constituency = .data$value_answering_member_member_from,
            answer_member_thumbnail_url = .data$value_answering_member_thumbnail_url)
    }

    if (sum(results$answer_is_correction, na.rm = TRUE) != 0) {
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

#' Format written question variable types
#'
#' \code{format_wq_variable_types} takes a tibble returned from \code{fetch_query}
#' and specifies variable data types.
#'
#' @param results The tibble returned from \code{fetch_query}.
#' @keywords internal

format_wq_variable_types <- function(results) {

    # Character
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(question_answer_id,
              question_member_mnis_id,
              question_answer_house,
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
              question_member_mnis_id_2,
              question_member_list_as,
              question_member_name,
              question_member_party,
              question_member_party_colour,
              question_member_party_abbreviation,
              question_member_constituency,
              question_member_thumbnail_url),
            as.character))

    if (all(is.na(results$answer_member_mnis_id)) == FALSE) {
        results <- results %>% dplyr::mutate(
            dplyr::across(
                c(answer_member_mnis_id_2,
                  answer_member_list_as,
                  answer_member_name,
                  answer_member_party,
                  answer_member_party_colour,
                  answer_member_party_abbreviation,
                  answer_member_constituency,
                  answer_member_thumbnail_url),
                as.character))
    }

    if (sum(results$answer_is_correction, na.rm = TRUE) != 0) {
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

    results <- results %>% dplyr::mutate_if(is.character, stringr::str_squish)

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

# Written statements ----------------------------------------------------------

#' Rename written statement variable names
#'
#' \code{format_ws_variable_names} takes a tibble returned from \code{process_pagination}
#' and renames variables with meaningful descriptions.
#'
#' @param results The tibble returned from \code{process_pagination}.
#' @keywords internal

format_ws_variable_names <- function(results) {

    results <- results %>% dplyr::rename(
        statement_links = .data$links,
        statement_id = .data$value_id,
        statement_member_mnis_id = .data$value_member_id,
        statement_member_role = .data$value_member_role,
        statement_uin = .data$value_uin,
        statement_date = .data$value_date_made,
        statement_body_id = .data$value_answering_body_id,
        statement_body_name = .data$value_answering_body_name,
        statement_title = .data$value_title,
        statement_text = .data$value_text,
        statement_house = .data$value_house,
        statement_notice_number = .data$value_notice_number,
        statement_has_attachments = .data$value_has_attachments,
        statement_has_linked_statements = .data$value_has_linked_statements,
        statement_linked_statements = .data$value_linked_statements,
        statement_attachments = .data$value_attachments,
        statement_member_mnis_id_2 = .data$value_member_id_2,
        statement_member_list_as = .data$value_member_list_as,
        statement_member_name = .data$value_member_name,
        statement_member_party = .data$value_member_party,
        statement_member_party_colour = .data$value_member_party_colour,
        statement_member_party_abbreviation = .data$value_member_party_abbreviation,
        statement_member_constituency = .data$value_member_member_from,
        statement_member_thumbnail_url = .data$value_member_thumbnail_url)

    results
}

#' Format written statement variable types
#'
#' \code{format_ws_variable_types} takes a tibble returned from \code{process_pagination}
#' and specifies variable data types.
#'
#' @param results The tibble returned from \code{process_pagination}.
#' @keywords internal

format_ws_variable_types <- function(results) {

    # Character
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(statement_id,
              statement_member_mnis_id,
              statement_member_role,
              statement_uin,
              statement_body_id,
              statement_body_name,
              statement_title,
              statement_text,
              statement_house,
              statement_notice_number,
              statement_member_mnis_id_2,
              statement_member_list_as,
              statement_member_name,
              statement_member_party,
              statement_member_party_colour,
              statement_member_party_abbreviation,
              statement_member_constituency,
              statement_member_thumbnail_url),
            as.character))

    results <- results %>% dplyr::mutate_if(is.character, stringr::str_squish)

    # List
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(statement_links,
              statement_attachments),
            as.list))

    # Logical
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(statement_has_attachments,
              statement_has_linked_statements),
            as.logical))

    # # Empty string -> NA
    # results$statement_linked_statements[lengths(results$statement_linked_statements) == 0] <- NA_character_
    # results[results == ""] <- NA

    # As Date
    results$statement_date <- as.Date(results$statement_date)

    # Return
    results
}

# Oral questions --------------------------------------------------------------

#' Format oral question variable names
#'
#' \code{format_oq_variable_names} takes a tibble returned from \code{process_oral_pagination}
#' and specifies variable data names.
#'
#' @param results The tibble returned from \code{process_oral_pagination}.
#' @keywords internal

format_oq_variable_names <- function(results) {

    results <- results %>% dplyr::rename(
        question_id = .data$id,
        question_type = .data$question_type,
        question_text = .data$question_text,
        question_status = .data$status,
        question_number = .data$number,
        question_date = .data$tabled_when,
        question_to_be_asked_when = .data$removed_from_to_be_asked_when,
        question_declarable_interest = .data$declarable_interest_detail,
        question_link = .data$hansard_link,
        question_uin = .data$uin,
        question_answer_date = .data$answering_when,
        question_answer_body_id = .data$answering_body_id,
        question_answer_body_name = .data$answering_body,
        question_answer_minister_title = .data$answering_minister_title,
        question_answer_minister_name = .data$answering_minister_name,
        question_member_id = .data$asking_member_id,
        question_answer_member_id = .data$answering_minister_id,
        question_member_mnis_id = .data$asking_member_mnis_id,
        question_member_pims_id = .data$asking_member_pims_id,
        question_member_name = .data$asking_member_name,
        question_member_list_as = .data$asking_member_list_as,
        question_member_constituency = .data$asking_member_constituency,
        question_member_status = .data$asking_member_status,
        question_member_party = .data$asking_member_party,
        question_member_party_id = .data$asking_member_party_id,
        question_member_party_colour = .data$asking_member_party_colour,
        question_member_photo_url = .data$asking_member_photo_url)

    results
}

#' Format oral question variable types
#'
#' \code{format_oq_variable_types} takes a tibble returned from \code{process_oral_pagination}
#' and specifies variable data types.
#'
#' @param results The tibble returned from \code{process_oral_pagination}.
#' @keywords internal

format_oq_variable_types <- function(results) {

    # Character
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(question_id,
              question_type,
              question_text,
              question_status,
              question_number,
              question_declarable_interest,
              question_link,
              question_uin,
              question_answer_body_id,
              question_answer_body_name,
              question_answer_minister_title,
              question_answer_minister_name,
              question_member_id,
              question_answer_member_id,
              question_member_mnis_id,
              question_member_pims_id,
              question_member_name,
              question_member_list_as,
              question_member_constituency,
              question_member_status,
              question_member_party,
              question_member_party_id,
              question_member_party_colour,
              question_member_photo_url),
            as.character))

    results <- results %>% dplyr::mutate_if(is.character, stringr::str_squish)

    # As Date
    results <- results %>% dplyr::mutate(
        dplyr::across(
            c(question_date,
              question_to_be_asked_when,
              question_answer_date),
            as.Date))

    # Return
    results
}

# Set house parameter ---------------------------------------------------------

#' Set house argument based on input
#'
#' \code{set_house} Sets the \code{house} argument based on input.
#'
#' @param house A string indicating Commons or Lords. Possible values include
#' "c", "C", "Commons", "l", "L", "Lords". Any other value will default to a
#' bicameral request.
#' @keywords internal

set_house <- function(house = NULL) {
    if (is.null(house)) {
        house <- "Bicameral"
    }
    if (house %in% c("c", "C", "Commons")) {
        house <- "Commons"
    } else if (house %in% c("l", "L", "Lords")) {
        house <- "Lords"
    } else {
        house <- "Bicameral"
    }
}
