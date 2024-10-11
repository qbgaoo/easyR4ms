test_that("filter_missing() Filter missing values with IDs", {
  conn <- x ~ id
  data <- data.frame(
    id = c(1:6),
    x = c(5.2, NA, 3.9, NA, 4.5, 4.3)
  )
  actual <- filter_missing(conn, data)
  expected <- data.frame(
    id = c(2, 4),
    x = as.numeric(c(NA, NA))
  )
  expect_equal(actual, expected, ignore_attr = TRUE)
})

