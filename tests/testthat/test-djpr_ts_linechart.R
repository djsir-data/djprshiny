library(ggplot2)
library(dplyr)

df <- economics_long %>%
  select(date, series = variable, value)

test_that("djpr_ts_linechart() works", {
  p_facet <- df %>%
    djpr_ts_linechart() +
    facet_wrap(~series, scales = "free")

  vdiffr::expect_doppelganger("faceted time series chart", p_facet)
})

test_that("djpr_ts_linechart() works with manual expansion", {
  p_exp <- df %>%
    filter(series == "unemploy") %>%
    djpr_ts_linechart(x_expand_mult = c(0, 0.4))

  vdiffr::expect_doppelganger("time series chart with more x space", p_exp)
})

test_that("djprtheme::get_plot_data() gets data from djpr_ts_linechart() plot", {
  p <- djpr_ts_linechart(df)

  p_data <- djprtheme::get_plot_data(p)

  expect_s3_class(p_data, "data.frame")
})

test_that("djpr_ts_linechart() respects label_single_line arg", {
  pce <- df %>%
    dplyr::filter(series == "pce")
  p1 <- djpr_ts_linechart(pce)
  p2 <- djpr_ts_linechart(pce,
    label_single_line = TRUE
  )

  vdiffr::expect_doppelganger(
    "regular time series plot",
    p1
  )

  vdiffr::expect_doppelganger(
    "time series plot with single series labelled",
    p2
  )
})
