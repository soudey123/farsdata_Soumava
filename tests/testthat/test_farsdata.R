context('reading data')

test_that('Simple test', {
  testthat::expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
})

context('File load and summary are correct')
test_that('Loads multiple years correctly', {
        dfs <- fars_read_years(2013:2015)
        testthat::expect_that(dfs, is_a('list'))
        expect_equal(length(dfs), 3)
})

test_that('FARS Summarize Years is correct', {
  years <- 2013:2015
  df <- fars_summarize_years(years)
  expect_that(nrow(df), equals(12))
  expect_that(ncol(df), equals(length(years) + 1))
  expect_that(names(df)[1], matches('MONTH'))
})
