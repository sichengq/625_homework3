test_that("my_lm detects singular fits", {
  # Create a perfectly collinear dataset
  singular_data <- data.frame(
    y = c(1, 2, 3),
    x1 = c(1, 1, 1),
    x2 = c(2, 2, 2)
  )

  # Test singular.ok = FALSE (should throw an error)
  expect_error(
    my_lm(y ~ x1 + x2, data = singular_data, singular.ok = FALSE),
    "Singular fit detected"
  )

  # Test singular.ok = TRUE (should proceed with a warning)
  expect_warning(
    my_lm(y ~ x1 + x2, data = singular_data, singular.ok = TRUE),
    "Singular fit detected"
  )
})
