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

test_that("multiple variable lm contrast, robust SE", {
  res <- linear_contrast(
    contr.names = c("x1", "x2"),
    contr.coef = c(1, -1),
    model = mod1,
    robust = TRUE,
    verbose = FALSE
  )
  expect_equal(length(res), 6)
})

x3 <- 20 * runif(100)
y2 <- rpois(100, lambda = rnorm(100, 10, 1) + 2.5 * x3)
mod2 <- glm(y2 ~ x3, family = poisson(link = "log"))

test_that("single variable glm contrast", {
  res <- linear_contrast(
    contr.names = c("x3"),
    model = mod2,
    transform = TRUE
  )
  expect_equal(length(res), 6)
})
test_that("single variable glm contrast, robust se", {
  res <- linear_contrast(
    contr.names = c("x3"),
    model = mod2,
    transform = TRUE,
    robust = TRUE
  )
  expect_equal(length(res), 6)
})
test_that("multiple variable glm contrast, robust se", {
  res <- linear_contrast(
    contr.names = c("x3", "(Intercept)"),
    contr.coef = c(1, -1),
    model = mod2,
    transform = TRUE,
    robust = TRUE
  )
  expect_equal(length(res), 6)
})
