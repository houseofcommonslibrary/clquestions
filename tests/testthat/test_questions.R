### Test committee functions
context("Question functions")

# Imports ---------------------------------------------------------------------

source("data.R")

# Setup -----------------------------------------------------------------------

fetch_questions_get <- read_data("fetch_questions_get")
fetch_questions_body_get <- read_data("fetch_questions_body_get")
fetch_questions_member_get <- read_data("fetch_questions_member_get")

fetch_questions_output <- read_data("fetch_questions_output")
fetch_questions_body_output <- read_data("fetch_questions_body_output")
fetch_questions_member_output <- read_data("fetch_questions_member_output")

fetch_answers_get <- read_data("fetch_answers_get")
fetch_answers_body_get <- read_data("fetch_answers_body_get")
fetch_answers_member_get <- read_data("fetch_answers_member_get")

fetch_answers_output <- read_data("fetch_answers_output")
fetch_answers_body_output <- read_data("fetch_answers_body_output")
fetch_answers_member_output <- read_data("fetch_answers_member_output")

# Mocks -----------------------------------------------------------------------

mock_fetch_questions_get <- function(url) {
    fetch_questions_get
}

test_that("fetch_written_questions processes results correctly.", {
    with_mock(
        "httr::GET" = mock_fetch_questions_get, {
            expected <- fetch_questions_output
            observed <- fetch_written_questions(on_date = QUESTION_DATE)
            expect_identical(observed, expected)
        })
})

