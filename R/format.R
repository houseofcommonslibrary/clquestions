### Functions for formatting column names

format_variable_names <- function(results) {

    results %>% dplyr::rename(
        question_links = .data$links,
        question_id = .data$value_id,
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
        answer_is_holding = .data$value_is_holding,
        asnswer_is_correction = .data$value_is_correction,
        answer_member_mnis_id = .data$value_answering_member_id,
        answer_correcting_member_mnis_id = .data$value_correcting_member_id,
        answer_correcting_member_name





    )





}
