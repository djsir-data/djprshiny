test_dates <- as.Date(c("1982-12-12",
                        "2006-02-14",
                      "1983-07-06",
                      "2017-10-18"))

test_that("date rounding functions work", {
  date_floor(test_dates) %>%
    expect_identical(as.Date(c("1982-12-01",
                               "2006-02-01",
                             "1983-07-01",
                             "2017-10-01")))

  date_ceiling(test_dates) %>%
    expect_identical(as.Date(c("1982-12-31",
                               "2006-02-28",
                               "1983-07-31",
                               "2017-10-31")))
})
