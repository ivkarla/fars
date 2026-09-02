context("fars functions")

test_that("make_filename builds the correct filename string", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename("2014"), "accident_2014.csv.bz2")
  expect_equal(make_filename(2015.0), "accident_2015.csv.bz2")
})

test_that("fars_read errors when the file does not exist", {
  expect_error(fars_read("this_file_does_not_exist.csv"))
})

test_that("fars_read_years returns NULL (with a warning) for an invalid year", {
  expect_warning(result <- fars_read_years(2099))
  expect_true(is.null(result[[1]]))
})
