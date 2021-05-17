test_that("extract_labs() behaves as expected", {
  library(ggplot2)
  library(patchwork)

  plot1 <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_point() +
    labs(subtitle = "Plot 1 subtitle",
         caption = "Plot 1 caption",
         title = "Plot 1 title")

  # Single plot
  expect_identical(extract_labs(plot1, "subtitle"), "Plot 1 subtitle")
  expect_identical(extract_labs(plot1, "caption"), "Plot 1 caption")
  expect_identical(extract_labs(plot1, "title"), "Plot 1 title")
  expect_error(extract_labs(plot1, "foobar"))

  # Create a second plot
  plot2 <- ggplot(economics, aes(x = date, y = uempmed)) +
    geom_point() +
    labs(subtitle = "Plot 2 subtitle")

  # Now combine these plots using patchwork
  comb_plots <- plot1 + plot2 +
    plot_annotation(title = "Combined plot title",
                    caption = "Data source")

  expect_identical(extract_labs(comb_plots, "title"), "Combined plot title")
  expect_identical(extract_labs(comb_plots, "caption"), "Data source")

  expect_error(extract_labs(LETTERS, "title"))
})
