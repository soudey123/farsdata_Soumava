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

