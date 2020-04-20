test_that("adding cumulative log column sums correctly", {
  credit_data <- tibble::tibble(
    work_unit = c("WU01", "WU02", "WU02", "WU02", "WU00"),
    folding_slot = c("FS01", "FS00", "FS00", "FS01", "FS00"),
    credits_attributed = c(65847, 6715, 1000, 9405, 4283),
    log_time = c("23:25:47", "04:36:53", "07:22:44", "13:27:32", "17:13:22"),
    log_date = structure(c(18348, 18349, 18349, 18349, 18349), class = "Date"),
    log_timestamp = structure(
      c(1585351547, 1585370213, 1585380164, 1585402052, 1585415602),
      tzone = "UTC", class = c("POSIXct","POSIXt"))
  )

  actual <- fahlogstats:::add_cumulative_sum(log_df = credit_data,
                                             sum_column = credits_attributed,
                                             date_column = log_date,
                                             folding_slot)

  expect_true("cumulative_credits_attributed" %in% colnames(actual))
  expect_equal(actual$cumulative_credits_attributed,
               c(6715, 7715, 11998, 65847, 75252))
})

test_that(
  "import & cleaning gives right date when the date rolls over at midnight", {
  log_file_path <- "./testdata/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  actual_out <-
    suppressMessages(
      suppressWarnings(
        actual_in %>%
          dplyr::mutate(log_df = purrr::map(log_file_name,
                                            read_log,
                                            log_file_path)) %>%
          fahlogstats::clean_logs()
      ))

  actual <-
    actual_out$log_timestamp %>%
    as.Date() %>%
    unique() %>%
    sort()

  expect_equal(actual,
               c(lubridate::ymd("2020-04-09"),
                 lubridate::ymd("2020-04-10"),
                 lubridate::ymd("2020-04-11"))
  )
})
