test_that("output is data.frame", {
  expect_s3_class(naPercentage(aqi_cities), "data.frame")
})

test_that("output data frame has same number of columns as input", {
  expect_equal(ncol(naPercentage(aqi_cities)), ncol(aqi_cities))
})
