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
    log_file_path <- "./testdata/test_process/"
    actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

    actual_out <- read_fah_logs(actual_in, log_file_path) %>% clean_logs()

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

test_that(
  "cleaning parses a log into the correct columns for a single work unit row", {
    actual_input <-
      tibble::tibble(log_file_name = "log-20200329-131612.txt",
                     message = "17:52:59:WU01:FS00:0xa7:Completed 92500 out of 125000 steps (74%)",
                     log_date = structure(18350, class = "Date"))

    actual_output <- clean_logs(actual_input)
    expect_equal(actual_output$`1`, "WU01")
    expect_equal(actual_output$`2`, "FS00")
    expect_equal(actual_output$`3`, "0xa7")
    expect_equal(actual_output$log_date, as.Date("2020-03-29"))
    expect_equal(actual_output$log_time, "17:52:59")
    expect_equal(actual_output$log_timestamp, lubridate::ymd_hms("2020-03-29 17:52:59"))
    expect_equal(nrow(actual_output), 1)
  })

test_that("daily network usage includes 0's for days when no usage ", {
  actual_input <-
    tibble::tibble(
      log_file_name = c("log.txt", "log.txt", "log.txt", "log.txt"),
      work_unit = c("WU00","WU00", "WU00", "WU00"),
      folding_slot = c("FS00", "FS00", "FS00", "FS00"),
      `3` = c("Downloading 1.0MiB",
              "Downloading 1.0MiB",
              "Downloading 1.0MiB",
              "Downloading 1.0MiB"),
      log_date = as.Date(c("2020-04-01",
                           "2020-04-01",
                           "2020-04-03",
                           "2020-04-03")),
      log_timestamp = lubridate::ymd_hms(c("2020-04-01 08:54:03",
                                           "2020-04-01 09:54:03",
                                           "2020-04-03 08:54:03",
                                           "2020-04-03 09:54:03")),
      log_time = c("08:54:03",
                   "09:54:03",
                   "08:54:03",
                   "09:54:03")
    )

  actual_output <-
    get_network_usage(actual_input) %>%
    calculate_daily_network_usage()

  testthat::expect_equal(
    actual_output$total_usage_mib,
    c(2, 0, 2)
  )

  testthat::expect_equal(
    unique(actual_output$log_date),
    as.Date(c("2020-04-01",
              "2020-04-02",
              "2020-04-03"))
  )
})

test_that("network usage parsing gets correct units", {
  actual_input <-
    tibble::tibble(
      log_file_name = c("log.txt", "log.txt", "log.txt"),
      work_unit = c("WU00","WU00", "WU00"),
      folding_slot = c("FS00", "FS00", "FS00"),
      `3` = c("Downloading 1.0MiB",
              "Downloading 1.0KiB",
              "Downloading 1.0GiB"),
      log_date = as.Date(c("2020-04-01",
                           "2020-04-02",
                           "2020-04-03")),
      log_timestamp = lubridate::ymd_hms(c("2020-04-01 08:54:03",
                                           "2020-04-02 09:54:03",
                                           "2020-04-03 08:54:03")),
      log_time = c("08:54:03",
                   "09:54:03",
                   "08:54:03")
    )

  actual_output <-
    get_network_usage(actual_input) %>%
    calculate_daily_network_usage()

  testthat::expect_equal(
    actual_output$total_usage_mib,
    c(1, 0.001, 1000)
  )
})

#########
# Slots #
#########
test_that("cpu and gpu slot names are retrieved.", {
  log_file_path <- "./testdata/test_process/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  logs_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs()

  actual <- get_folding_slot_names(logs_df)


  testthat::expect_equal(actual$processor_name %>% sort(),
                         c("AMD Ryzen 5 2600 Six-Core Processor",
                           "Vega 10 XL/XT [Radeon RX Vega 56/64]"))

  testthat::expect_equal(actual$processor_type %>% sort(),
                         c("CPU",
                           "GPU"))

})

test_that("progress is 100 for each slot that has no current work", {
  log_file_path <- "./testdata/test_process/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  logs_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs()

  slot_progress <-
    logs_df %>%
    get_slot_progress() %>%
    dplyr::arrange(slot_progress)

  actual <- slot_progress$slot_progress
  testthat::expect_equal(actual, c(100, 100))
})

test_that("progress is NA when no work has completed or started", {
  log_file_path <- "./testdata/test_process/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  live_logs_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs() %>%
    head(300)

  slot_progress <-
    live_logs_df %>%
    get_slot_progress() %>%
    dplyr::arrange(slot_progress)

  actual <- slot_progress$slot_progress
  testthat::expect_equal(actual, c(50, NA))
})

##############
# Work Units #
##############


test_that("the latest work unit details are correct", {
  log_file_path <- "./testdata/test_process/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  live_logs_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs()

  actual <-
    live_logs_df %>%
    get_latest_work_unit_details()

  testthat::expect_equal(colnames(actual), c("folding_slot", "latest_credits_acquired", "latest_work_unit_duration"))
})
