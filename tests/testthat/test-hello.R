context("test-hello")

test_that("Hello is printed", {
  x <- print("Hello, world!")
  z <- hello()
  expect_identical(x, z)
})
