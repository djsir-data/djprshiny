test_that("centred_row() fails with no content", {
  expect_error(centred_row())
})

test_that("centred_row() returns expected output", {
  empty_content <- centred_row("")
  empty_content_char <- as.character(empty_content)

  expect_s3_class(empty_content, "shiny.tag")
  expect_true(grepl("div", empty_content_char))
})

test_that("centred_row() only accepts appropriate col sizes", {
  # First check default widths return a shiny.tag object
  expect_s3_class(
    centred_row(shiny::plotOutput("plot1")),
    "shiny.tag"
  )

  # All widths must be > 1
  expect_error(centred_row("", left_space = 0))
  expect_error(centred_row("", right_space = 0))

  # This leaves 0 space for the centre row
  expect_error(centred_row("", left_space = 6, right_space = 6))

  # All individual widths must be <= 10
  expect_error(centred_row("", left_space = 11, right_space = 1))
})
