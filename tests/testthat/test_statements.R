### Test statement functions
context("Statement functions")

# Imports ---------------------------------------------------------------------

source("data.R")

# Setup -----------------------------------------------------------------------

fetch_statements_get <- read_data("fetch_statements_get")
fetch_statements_body_get <- read_data("fetch_statements_body_get")
fetch_statements_member_get <- read_data("fetch_statements_member_get")

fetch_statements_output <- read_data("fetch_statements_output")
fetch_statements_body_output <- read_data("fetch_statements_body_output")
fetch_statements_member_output <- read_data("fetch_statements_member_output")

# Mocks -----------------------------------------------------------------------

mock_fetch_statements_get <- function(url) {
    fetch_statements_get
}

mock_fetch_statements_body_get <- function(url) {
    fetch_statements_body_get
}

mock_fetch_statements_member_get <- function(url) {
    fetch_statements_member_get
}

# Test fetch statements -------------------------------------------------------

test_that("fetch_written_statements processes results correctly.", {
    with_mock(
        "httr::GET" = mock_fetch_statements_get, {
            expected <- fetch_statements_output
            observed <- fetch_written_statements(on_date = STATEMENT_DATE, take = TAKE_STATEMENTS)
            expect_identical(observed, expected)
        })
})

test_that("fetch_written_statements_body processes results correctly.", {
    with_mock(
        "httr::GET" = mock_fetch_statements_body_get, {
            expected <- fetch_statements_body_output
            observed <- fetch_written_statements_body(body_id = STATEMENT_BODY, on_date = STATEMENT_DATE, take = TAKE_STATEMENTS)
            expect_identical(observed, expected)
        })
})

test_that("fetch_written_statements_member processes results correctly.", {
    with_mock(
        "httr::GET" = mock_fetch_statements_member_get, {
            expected <- fetch_statements_member_output
            observed <- fetch_written_statements_member(member_mnis_id = STATEMENT_MEMBER, on_date = STATEMENT_DATE, take = TAKE_STATEMENTS)
            expect_identical(observed, expected)
        })
})
