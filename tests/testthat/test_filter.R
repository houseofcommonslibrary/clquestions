### Test filter functions
context("Filter functions")

# Test data -------------------------------------------------------------------

mem_a_csv <- "
    mnis_id,    start_date,     end_date
    p1,         2001-01-01,     2001-12-31
    p1,         2005-01-01,     2005-12-31
    p1,         2006-01-01,     2006-12-31
    p1,         2010-01-01,     2010-12-31
    p2,         2005-01-01,     2005-12-31
    p2,         2006-01-01,     2006-12-31
    p2,         2010-01-01,     2010-12-31
    p2,         2015-01-01,     2015-12-31
"

mem_b_csv <- "
    mnis_id,    start_date,     end_date
    p1,         2001-06-01,     2002-06-30
    p1,         2004-01-01,     2004-12-31
    p1,         2006-01-01,     2006-12-31
    p1,         2011-01-01,     2011-12-31
    p2,         2004-01-01,     2004-12-31
    p2,         2006-01-01,     2006-12-31
    p2,         2011-01-01,     2011-12-31
    p2,         2015-06-01,     2016-06-30
"

mem_a <- readr::read_csv(
    mem_a_csv,
    trim_ws = TRUE,
    col_types = readr::cols(
        mnis_id = readr::col_character(),
        start_date = readr::col_date(),
        end_date = readr::col_date()))

mem_b <- readr::read_csv(
    mem_b_csv,
    trim_ws = TRUE,
    col_types = readr::cols(
        mnis_id = readr::col_character(),
        start_date = readr::col_date(),
        end_date = readr::col_date()))

# Test filter_dates -----------------------------------------------------------

test_that("filter_dates raises a missing column error.", {

    expect_error(
        filter_dates(
            mem_a,
            start_col = "no_such_column",
            end_col = "end_date"),
        "Could not find a column called no_such_column")

    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "no_such_column"),
        "Could not find a column called no_such_column")
})

test_that("filter_dates raises an error when dates are out of sequence.", {
    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "end_date",
            from_date = "2010-01-01",
            to_date = "2009-12-31"),
        "to_date is before from_date")
})

test_that("filter_dates raises a date format error.", {

    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "end_date",
            from_date = "2010-01-XX",
            to_date = "2010-12-31"),
        "2010-01-XX is not a valid Date or date string")

    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "end_date",
            from_date = "2010-01-01",
            to_date = "2010-12-XX"),
        "2010-12-XX is not a valid Date or date string")
})

test_that("filter_dates does not filter without dates.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date")

    expect_equal(nrow(f_mem_a), nrow(mem_a))
    expect_equal(ncol(f_mem_a), ncol(mem_a))
    expect_equal(all(f_mem_a == mem_a), TRUE)
})

test_that("filter_dates excludes rows before from_date.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2004-12-31")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 1)
    expect_equal(ncol(f_mem_a), ncol(mem_a))
    expect_equal(f_mem_a[1, ]$mnis_id, "p1")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))
})

test_that("filter_dates excludes rows after to_date.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        to_date = "2011-01-01")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 1)
    expect_equal(ncol(f_mem_a), ncol(mem_a))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$mnis_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2010-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2010-12-31"))
})

test_that("filter_dates excludes rows outside both dates.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2004-12-31",
        to_date = "2011-01-01")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 2)
    expect_equal(ncol(f_mem_a), ncol(mem_a))

    expect_equal(f_mem_a[1, ]$mnis_id, "p1")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))

    expect_equal(f_mem_a[nrow(f_mem_a), ]$mnis_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2010-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2010-12-31"))
})

test_that("filter_dates includes rows with partial instersection.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2005-06-30",
        to_date = "2010-06-30")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 2)
    expect_equal(ncol(f_mem_a), ncol(mem_a))

    expect_equal(f_mem_a[1, ]$mnis_id, "p1")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))

    expect_equal(f_mem_a[nrow(f_mem_a), ]$mnis_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2010-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2010-12-31"))
})

test_that("filter_dates includes rows with enclosing dates.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2005-06-30",
        to_date = "2005-06-30")

    expect_equal(nrow(f_mem_a), 2)
    expect_equal(ncol(f_mem_a), ncol(mem_a))

    expect_equal(f_mem_a[1, ]$mnis_id, "p1")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))

    expect_equal(f_mem_a[nrow(f_mem_a), ]$mnis_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2005-12-31"))
})
