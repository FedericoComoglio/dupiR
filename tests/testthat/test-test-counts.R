# constructor ------------------------------------------------------------

test_that("counts mandatory", {
  expect_error(new_counts(fractions = c(0.1, 0.2)),
                          regexp = "The number of measurements does not match the number of fractions")
})

test_that("fractions mandatory", {
  expect_error(new_counts(counts = c(30L, 35L)),
                          regexp = "The number of measurements does not match the number of fractions")
})

test_that("fractions strictly positive", {
  expect_error(new_counts(counts = c(30L, 35L),
                          fractions = c(0, 0.1)),
                          regexp = "At least one fraction not")
})

test_that("fractions strictly positive", {
  expect_error(new_counts(counts = c(30L, 35L),
                          fractions = c(-1, 0.1)),
                          regexp = "At least one fraction not")
})

test_that("fractions smaller than or equal to 1", {
  expect_error(new_counts(counts = c(30L, 35L),
                          fractions = c(1.1, 0.1)),
                          regexp = "At least one fraction not")
})

test_that("numeric counts casted to integers", {
  expect_message(new_counts(counts = c(11, 20),
                          fractions = c(0.1, 0.2)),
               regexp = "Counts converted to integer")
})



