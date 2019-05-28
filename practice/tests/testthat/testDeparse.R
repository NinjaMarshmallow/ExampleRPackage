test_that("Test Deparse2", {
  a <- 10
  b <- 9
  expect_equal(deparse2(a + b), "19")
  expect_equal(deparse2(1 + 2 + 3 + 4 + 5 + a + 6 + b + 3 + b + 7 + a + 3 * a), "99")
  expect_equal(deparse2(substitute(a + b)), "10 + 9")
  expect_equal(deparse2(
    substitute(
      a + b + a + b + a + b + a + b + a + b + a + b + a + b + a + b + a + b + a + b + a + b + a + b)),
    "a + b + a + b + a + b + a + b + a + b + a + b + a + b + a + b +     a + b + a + b + a + b + a + b")
})
