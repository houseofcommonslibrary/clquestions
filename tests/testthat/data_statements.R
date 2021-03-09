### Record and retrieve test data: statements

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

fetch_statements_data <- function() {

    url_statements <- stringr::str_glue(stringr::str_c(
        WS_BASE_URL,
        "&take={TAKE_STATEMENTS}",
        "&house={HOUSE}",
        "&madeWhenFrom={STATEMENT_DATE}",
        "&madeWhenTo={STATEMENT_DATE}"))

    url_statements_body <- stringr::str_glue(stringr::str_c(
        WS_BASE_URL,
        "&take={TAKE_STATEMENTS}",
        "&house={HOUSE}",
        "&answeringBodies={STATEMENT_BODY}",
        "&madeWhenFrom={STATEMENT_DATE}",
        "&madeWhenTo={STATEMENT_DATE}"))

    url_statements_member <- stringr::str_glue(stringr::str_c(
        WS_BASE_URL,
        "&take={TAKE_STATEMENTS}",
        "&house={HOUSE}",
        "&members={STATEMENT_MEMBER}",
        "&madeWhenFrom={STATEMENT_DATE}",
        "&madeWhenTo={STATEMENT_DATE}"))

    # Fetch data
    fetch_statements_get <- httr::GET(url_statements)
    fetch_statements_body_get <- httr::GET(url_statements_body)
    fetch_statements_member_get <- httr::GET(url_statements_member)

    fetch_statements_output <- fetch_written_statements(on_date = STATEMENT_DATE)
    fetch_statements_body_output <- fetch_written_statements_body(STATEMENT_BODY, on_date = STATEMENT_DATE)
    fetch_statements_member_output <- fetch_written_statements_member(STATEMENT_MEMBER, on_date = STATEMENT_DATE)

    # Write data
    write_data(fetch_statements_get, "fetch_statements_get")
    write_data(fetch_statements_body_get, "fetch_statements_body_get")
    write_data(fetch_statements_member_get, "fetch_statements_member_get")

    write_data(fetch_statements_output, "fetch_statements_output")
    write_data(fetch_statements_body_output, "fetch_statements_body_output")
    write_data(fetch_statements_member_output, "fetch_statements_member_output")

}

# Fetch all memberships test data ---------------------------------------------

fetch_statements_data()
message("API output recorded for statements")
