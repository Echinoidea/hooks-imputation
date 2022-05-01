# Is output a ggplot
test_that("is of class 'gg' and 'ggplot'", {
  expect_s3_class(compareDistribution(aqi_cities, aqi_cities_imp, by = "2020"), c('gg', 'ggplot'))
})
