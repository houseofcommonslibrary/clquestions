### Test API query functions
context("Query API function")

# Questions and answers query
qa_query_cols <- c(
    "links",
    "value_id",
    "value_asking_member_id",
    "value_house",
    "value_member_has_interest",
    "value_date_tabled",
    "value_date_for_answer",
    "value_uin",
    "value_question_text",
    "value_answering_body_id",
    "value_answering_body_name",
    "value_is_withdrawn",
    "value_is_named_day",
    "value_grouped_questions",
    "value_answer_is_holding",
    "value_answer_is_correction",
    "value_answering_member_id",
    "value_correcting_member_id",
    "value_correcting_member",
    "value_date_answered",
    "value_answer_text",
    "value_original_answer_text",
    "value_comparable_answer_text",
    "value_date_answer_corrected",
    "value_date_holding_answer",
    "value_attachment_count",
    "value_heading",
    "value_attachments",
    "value_grouped_questions_dates", # Added 2021-03-09
    "value_asking_member_id_2",
    "value_asking_member_list_as",
    "value_asking_member_name",
    "value_asking_member_party",
    "value_asking_member_party_colour",
    "value_asking_member_party_abbreviation",
    "value_asking_member_member_from",
    "value_asking_member_thumbnail_url",
    "value_answering_member_id_2",
    "value_answering_member_list_as",
    "value_answering_member_name",
    "value_answering_member_party",
    "value_answering_member_party_colour",
    "value_answering_member_party_abbreviation",
    "value_answering_member_member_from",
    "value_answering_member_thumbnail_url")

# Statement query
s_query_cols <- c(
    "links",
    "value_id",
    "value_member_id",
    "value_member_role",
    "value_uin",
    "value_date_made",
    "value_answering_body_id",
    "value_answering_body_name",
    "value_title",
    "value_text",
    "value_house",
    "value_notice_number",
    "value_has_attachments",
    "value_has_linked_statements",
    "value_linked_statements",
    "value_attachments",
    "value_member_id_2",
    "value_member_list_as",
    "value_member_name",
    "value_member_party",
    "value_member_party_colour",
    "value_member_party_abbreviation",
    "value_member_member_from",
    "value_member_thumbnail_url")

# Tests -----------------------------------------------------------------------

test_that("query_results sends and recieves basic written question and answer query", {

    # Fetch data
    response <- httr::GET(WQ_BASE_URL)
    response_text <- httr::content(response, as = "text", encoding = "utf-8")
    response_text <- response_text %>% jsonlite::fromJSON(flatten = TRUE)
    response_results <- response_text$results %>%
        tibble::as_tibble() %>%
        janitor::clean_names()

    expect_equal(response$status_code, 200)
    expect_equal(ncol(response_results), 45)
    expect_equal(colnames(response_results), qa_query_cols)
})

test_that("query_results sends and recieves basic statements query", {

    # Fetch data
    response <- httr::GET(WS_BASE_URL)
    response_text <- httr::content(response, as = "text", encoding = "utf-8")
    response_text <- response_text %>% jsonlite::fromJSON(flatten = TRUE)
    response_results <- response_text$results %>%
        tibble::as_tibble() %>%
        janitor::clean_names()

    expect_equal(response$status_code, 200)
    expect_equal(ncol(response_results), 24)
    expect_equal(colnames(response_results), s_query_cols)
})
