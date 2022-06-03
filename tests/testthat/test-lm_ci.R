x1 <- rnorm(100, 5, 2)
x2 <- runif(100, 3, 12)
y <- 25 + x1 - 3*x2 + rnorm(100)
mod1 <- lm(y ~ x1 + x2)

test_that("lm_ci base case", {
  res <- lm_ci(mod1)
  expect_equal(dim(res), c(3, 5))
})
test_that("lm_ci base case with robust se", {
  res <- lm_ci(mod1, robust = TRUE)
  expect_equal(dim(res), c(3, 5))
})
test_that("lm_ci base case with robust se and exponentiated coefficients", {
  res <- lm_ci(mod1, robust = TRUE, expcoef = TRUE)
  expect_equal(dim(res), c(3, 5))
})


y2 <- rpois(100, lambda = 30 + x1 + 3*x2 + rnorm(100))
mod2 <- glm(y2 ~ x1 + x2, family = poisson)
test_that("glm_ci base case", {
  res <- glm_ci(mod2)
  expect_equal(dim(res), c(3, 5))
})
test_that("glm_ci base case with robust se", {
  res <- glm_ci(mod2, robust = TRUE)
  expect_equal(dim(res), c(3, 5))
})
test_that("glm_ci base case with robust se and non-exponentiated coefficients", {
  res <- glm_ci(mod2, robust = TRUE, transform = FALSE)
  expect_equal(dim(res), c(3, 5))
})

