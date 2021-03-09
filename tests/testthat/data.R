### Record and retrieve test data: helper functions

# About -----------------------------------------------------------------------

# The functions in this file are used to record the output of the api and the
# functions that process that data in order to produce mocks, and to check if
# the expected behaviour of the functions has changed. The file paths are set
# in here so that you can source files for creating test data from within the
# package project during development, and source the test data files from
# within the tests.

# Constants -------------------------------------------------------------------

READ_TEST_DIR <- file.path("data")
WRITE_TEST_DIR <- file.path("tests", "testthat", "data")

# Written questions and answers
QUESTION_DATE <- "2021-02-01"
ANSWER_DATE <- "2021-02-01"
QUESTION_MEMBER <- 4651 # Dan Carden
ANSWER_MEMBER <- 4485 # Johnny Mercer
ANSWER_BODY <- 11 # Ministry of Defence
TAKE_QUESTIONS <- 1000

# Written statements
STATEMENT_DATE <- "2021-02-01"
STATEMENT_MEMBER <- 4066 # Priti Patel
STATEMENT_BODY <- 1 # Home Office
TAKE_STATEMENTS <- 20

HOUSE <- "Bicameral"

# Read and write data ---------------------------------------------------------

# Read a file from the data directory
read_data <- function(filename) {
    readRDS(file.path(READ_TEST_DIR,
                      stringr::str_glue("{filename}.RData")))
}

# Write R data to the data directory
write_data <- function(data, filename) {
    saveRDS(data, file.path(WRITE_TEST_DIR,
                            stringr::str_glue("{filename}.RData")))
}

# Mocks -----------------------------------------------------------------------

# Mocks a call to httr::GET which returns the given response
get_mock_get <- function(response) {
    function(url) {response}
}
