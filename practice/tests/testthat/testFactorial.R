
test_that("Test Factorial Function", {
  expect_equal(factorial2(0), 1)
  expect_equal(factorial2(1), 1)
  expect_equal(factorial2(2), 2)
  expect_equal(factorial2(3), 6)
  expect_equal(factorial2(4), 24)
  expect_equal(factorial2(5), 120)
  expect_equal(factorial2(6), 720)
  expect_error(factorial2(-1))
  expect_error(factorial2("String"))
  expect_equal(c(1, 6, 720), factorial2(c(1, 3, 6)))
})
