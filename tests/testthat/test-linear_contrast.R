x1 <- rnorm(100)
x2 <- rexp(100)
y <- 3 * x1 + 2 * x2 + rnorm(100, sd = 0.25)
mod1 <- lm(y ~ x1 + x2)

test_that("single variable lm contrast", {
  res <- linear_contrast(
    contr.names = "x1",
    model = mod1
  )
  expect_equal(length(res), 6)
})

test_that("single variable lm contrast, robust SE", {
  res <- linear_contrast(
    contr.names = "x1",
    model = mod1,
    robust = TRUE,
    verbose = FALSE
  )
  expect_equal(length(res), 6)
})
