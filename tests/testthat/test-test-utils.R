# computing ecdf ------------------------------------------------------------

test_that("ecdf computed as cumulative sum", {
  expect_equal(compute_ecdf(c(1:3)), c(1, 3, 6))
})




