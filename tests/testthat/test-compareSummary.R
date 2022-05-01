# Is output a data frame
test_that("output is data.frame", {
  expect_s3_class(compareSummary(aqi_cities, aqi_cities_imp, by = "X2020"), "data.frame")
})

# Does output have dimensions (2, 3)
test_that("output has 2 rows and 3 columns", {
  expect_equal(dim(compareSummary(aqi_cities, aqi_cities_imp, by = "X2020")), c(2, 3))
})
