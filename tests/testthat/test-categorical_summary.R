x <- sample(c("a", "b", "c"), size = 400, replace = TRUE, prob = c(0.1, 0.5, 0.4))

test_that("basic categorical test", {
  res <- categorical_summary(x, "a")
  expect_equal(class(res), "character")
})

test_that("basic categorical test 2", {
  res <- categorical_summary(x, "c")
  expect_equal(class(res), "character")
})
