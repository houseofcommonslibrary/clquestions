### Record and retrieve test data: committee functions

# About -----------------------------------------------------------------------

# WARNING: RUNNING THIS FILE WILL REBUILD THE TEST DATA FOR THESE FUNCTIONS
#
# The functions in this file are used to record the output of the api and the
# functions that process that data in order to produce mocks, and to check if
# the expected behaviour of the functions has changed. The file paths are set
# so that you can source this file from within the package project during
# development to generate the test data, and source the test data files from
# within the corresponding tests. Only run this run file when you are ready
# to capture current behaviour.

# Imports ---------------------------------------------------------------------

source("tests/testthat/data.R")

# Fetch test data for core functions ------------------------------------------

fetch_questions_data <- function() {

    url_questions <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&take={TAKE}",
        "&house={HOUSE}",
        "&tabledWhenFrom={QUESTION_DATE}",
        "&tabledWhenTo={QUESTION_DATE}"))

    url_questions_body <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&take={TAKE}",
        "&house={HOUSE}",
        "&answeringBodies={ANSWER_BODY}",
        "&tabledWhenFrom={QUESTION_DATE}",
        "&tabledWhenTo={QUESTION_DATE}"))

    url_questions_member <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&take={TAKE}",
        "&house={HOUSE}",
        "&askingMemberId={QUESTION_MEMBER}",
        "&tabledWhenFrom={QUESTION_DATE}",
        "&tabledWhenTo={QUESTION_DATE}"))

    url_answers <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&take={TAKE}",
        "&house={HOUSE}",
        "&answeredWhenFrom={ANSWER_DATE}",
        "&answeredWhenTo={ANSWER_DATE}"))

    url_answers_body <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&take={TAKE}",
        "&house={HOUSE}",
        "&answeringBodies={ANSWER_BODY}",
        "&answeredWhenFrom={ANSWER_DATE}",
        "&answeredWhenTo={ANSWER_DATE}"))

    url_answers_member <- stringr::str_glue(stringr::str_c(
        WQ_BASE_URL,
        "&take={TAKE}",
        "&house={HOUSE}",
        "answeringMemberId={ANSWER_MEMBER}",
        "&answeredWhenFrom={ANSWER_DATE}",
        "&answeredWhenTo={ANSWER_DATE}"))

    # Fetch data
    fetch_questions_get <- httr::GET(url_questions)
    fetch_questions_body_get <- httr::GET(url_questions_body)
    fetch_questions_member_get <- httr::GET(url_questions_member)

    fetch_questions_output <- fetch_written_questions(on_date = QUESTION_DATE)
    fetch_questions_body_output <- fetch_written_questions_body(ANSWER_BODY, on_date = QUESTION_DATE)
    fetch_questions_member_output <- fetch_written_questions_member(QUESTION_MEMBER, on_date = QUESTION_DATE)

    fetch_answers_get <- httr::GET(url_answers)
    fetch_answers_body_get <- httr::GET(url_answers_body)
    fetch_answers_member_get <- httr::GET(url_answers_member)

    fetch_answers_output <- fetch_written_answers(on_date = ANSWER_DATE)
    fetch_answers_body_output <- fetch_written_answers_body(ANSWER_BODY, on_date = ANSWER_DATE)
    fetch_answers_member_output <- fetch_written_answers_member(ANSWER_MEMBER, on_date = ANSWER_DATE)

    # Write data
    write_data(fetch_questions_get, "fetch_questions_get")
    write_data(fetch_questions_body_get, "fetch_questions_body_get")
    write_data(fetch_questions_member_get, "fetch_questions_member_get")

    write_data(fetch_questions_output, "fetch_questions_output")
    write_data(fetch_questions_body_output, "fetch_questions_body_output")
    write_data(fetch_questions_member_output, "fetch_questions_member_output")

    write_data(fetch_answers_get, "fetch_answers_get")
    write_data(fetch_answers_body_get, "fetch_answers_body_get")
    write_data(fetch_answers_member_get, "fetch_answers_member_get")

    write_data(fetch_answers_output, "fetch_answers_output")
    write_data(fetch_answers_body_output, "fetch_answers_body_output")
    write_data(fetch_answers_member_output, "fetch_answers_member_output")

}

# Fetch all memberships test data ---------------------------------------------

fetch_questions_data()
message("API output recorded for questions and answers")
