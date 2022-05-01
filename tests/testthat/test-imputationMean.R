# Is output of type list
test_that("is of type list", {
  expect_type(imputationMean(aqi_cities,
                             by.row = TRUE,
                             col.range = 2:ncol(aqi_cities)),
              'list')
})

# Does output contain any NA values
test_that("result contains NA values", {
  expect_failure(expect_na(imputationMean(aqi_cities,
                                          by.row = TRUE,
                                          col.range = 2:ncol(aqi_cities))))
})

# Is output the same size as input
test_that("dim(output) == dim(input)", {
  expect_equal(dim(imputationMean(aqi_cities,
                                  by.row = TRUE,
                                  col.range = 2:ncol(aqi_cities))),
               dim(aqi_cities))
})
