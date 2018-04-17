context('reading and summarizing data')

test_that('Simple test', {
  testthat::expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
})

test_that("import the fars data", {
  data_path <- system.file("extdata", "accident_2013.csv.bz2", package = "farsdata")
  dat       <- fars_read(data_path)
  testthat::expect_that(dat, is_a('tbl_df'))
  testthat::expect_equal(nrow(dat), 30202)
})
