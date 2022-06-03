test_that("mean works", {
  x <- rnorm(100, 50, 34)
  res <- continuous_summary(x)
  expect_equal(class(res), "character")
})

test_that("median works", {
  x <- rnorm(100, 50, 34)
  res <- continuous_summary(x, type = "median")
  expect_equal(class(res), "character")
})

test_that("subset works", {
  x <- rnorm(100, 50, 34)
  subset <- x > 50
  res <- continuous_summary(x, subset = subset, type = "median")
  expect_equal(class(res), "character")
})