test_that("calc_hom works", {

  expect_error(calc_hom(counts = "Hello"))

  mat <- matrix(1:12, ncol = 3)

  actual <- calc_hom(mat, normalise = FALSE)
  expected <- c(0.85323615, 0.93688831, 0.98290073, 1.01140426)
  expect_equal(actual, expected)

  actual <- calc_hom(mat, normalise = TRUE)
  expected <- c(0.61547978, 0.67582206, 0.7090130, 0.72957396)
  expect_equal(actual, expected)

})
