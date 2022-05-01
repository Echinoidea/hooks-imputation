# Is output of type list
test_that("is of type list", {
  expect_type(imputationMice(aqi_cities[1:10,],
                             by.row = TRUE,
                             col.range = 2:ncol(aqi_cities),
                             printFlag = FALSE),
              'list')
})

# Does output contain any NA values
test_that("result contains NA values", {
  expect_failure(expect_na(imputationMice(aqi_cities[1:10,],
                                          by.row = TRUE,
                                          col.range = 2:ncol(aqi_cities),
                                          printFlag = FALSE)))
})

# Is output the same size as input
test_that("dim(output) == dim(input)", {
  expect_equal(dim(imputationMice(aqi_cities[1:10,],
                                  by.row = TRUE,
                                  col.range = 2:ncol(aqi_cities),
                                  printFlag = FALSE)),
               dim(aqi_cities[1:10, 2:ncol(aqi_cities)]))
})

# aqi_cities is shortened to first 10 rows because mice takes a really long
# time to process. This should still show if there are any issues in the function.
