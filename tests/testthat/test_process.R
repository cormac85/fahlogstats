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

test_that("spurious CPU / cores don't add spurious folding slots", {
  actual_input <-
    structure(list(log_file_name = "log.txt", `1` = "WU00", `2` = "FS01",
                   `3` = "0x21", `4` = "CPU", `5` = " 0x00000000000000000000000000000000",
                   `6` = NA_character_, `7` = NA_character_, `8` = NA_character_,
                   `9` = NA_character_, `10` = NA_character_, `11` = NA_character_,
                   `12` = NA_character_, `13` = NA_character_, log_date = structure(18407, class = "Date"),
                   log_row_index = 8039L, log_time = "03:04:02", log_timestamp = structure(1590375842, class = c("POSIXct",
                                                                                                                 "POSIXt"), tzone = "UTC"), lagged_time_diff = structure(0, class = "difftime", units = "secs"),
                   is_date_rollover = 3L), row.names = c(NA, -1L), class = c("tbl_df",
                                                                             "tbl", "data.frame")) %>%
    dplyr::union_all(
      structure(list(log_file_name = "log.txt", `1` = "WU00", `2` = "FS00",
                     `3` = "0xa7", `4` = "        CPU", `5` = " AMD Ryzen 5 2600 Six-Core Processor",
                     `6` = NA_character_, `7` = NA_character_, `8` = NA_character_,
                     `9` = NA_character_, `10` = NA_character_, `11` = NA_character_,
                     `12` = NA_character_, `13` = NA_character_, log_date = structure(18404, class = "Date"),
                     log_row_index = 233L, log_time = "18:03:28", log_timestamp = structure(1590170608, tzone = "UTC", class = c("POSIXct",
                                                                                                                                 "POSIXt")), lagged_time_diff = structure(0, class = "difftime", units = "secs"),
                     is_date_rollover = 0L), row.names = c(NA, -1L), class = c("tbl_df",
                                                                               "tbl", "data.frame"))
    )

  actual <- get_folding_slot_names(actual_input)

  testthat::expect_equal(nrow(actual), 1)
  testthat::expect_equal(actual$processor_name,
                         "AMD Ryzen 5 2600 Six-Core Processor")

})


test_that("at the start of logs that the GPU slot name is not replaced by the CPU slot name", {
  log_file_path <- "./testdata/test_duplicate_slots/"
  actual_in <- tibble::tibble(log_file_name = "log_duplicate_slots-20200409-124015.txt")


  actual <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs() %>%
    get_folding_slot_names()

  testthat::expect_equal(nrow(actual), 2)
  testthat::expect_equal(
    actual$processor_name,
    c("AMD Ryzen 5 2600 Six-Core Processor",
      "Vega 10 XL/XT [Radeon RX Vega 56/64]")
  )

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

test_that("progress duration is correct", {
  log_file_path <- "./testdata/test_process_overlapping_wu/"
  actual_in <- tibble::tibble(log_file_name = "log-20200517-124015.txt")

  live_logs_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs()

  slot_progress <-
    live_logs_df %>%
    get_slot_progress() %>%
    dplyr::arrange(slot_progress)

  actual <- slot_progress$progress_timestamp - slot_progress$work_start
  testthat::expect_equal(actual,
                         c(difftime(time1 = "2020-05-17 09:49:42",
                                    time2= "2020-05-17 07:29:53"),
                           difftime(time1 = "2020-05-17 09:48:28",
                                    time2 = "2020-05-17 07:38:08")),
                         tolerance = 0.01)
})

##############
# Work Units #
##############


test_that("the latest work unit columns are correct", {
  log_file_path <- "./testdata/test_process/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  work_units_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs() %>%
    get_work_unit_data()

  actual <- get_latest_work_unit_details(work_units_df)

  testthat::expect_equal(colnames(actual), c("folding_slot",
                                             "work_unit",
                                             "work_start",
                                             "work_end",
                                             "latest_work_duration",
                                             "latest_credits_attributed"))
})

test_that("the latest work unit durations are correct", {
  log_file_path <- "./testdata/test_process/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  work_units_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs() %>%
    get_work_unit_data()

  actual <- get_latest_work_unit_details(work_units_df)

  testthat::expect_equal(actual$latest_work_duration,
                         c(difftime(time1 = "2020-04-10 14:46:55",
                                    time2= "2020-04-10 12:59:56"),
                           difftime(time1 = "2020-04-10 07:18:28",
                                    time2= "2020-04-10 05:32:20")))

})

test_that("the latest work unit credits are correct", {
  log_file_path <- "./testdata/test_process/"
  actual_in <- tibble::tibble(log_file_name = "log-20200409-124015.txt")

  work_units_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs() %>%
    get_work_unit_data()

  actual <- get_latest_work_unit_details(work_units_df)

  testthat::expect_equal(actual$latest_credits_attributed,
                         c(4402, 69338))

})

test_that("the latest work unit durations are correct when wu's overlap", {
  log_file_path <- "./testdata/test_process_overlapping_wu/"
  actual_in <- tibble::tibble(log_file_name = "log-20200517-124015.txt")

  work_units_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs() %>%
    get_work_unit_data()

  actual <- get_latest_work_unit_details(work_units_df)

  testthat::expect_equal(actual$latest_work_duration,
                         c(difftime(time1 = "2020-05-17 07:29:58",
                                    time2= "2020-05-17 04:40:56"),
                           difftime(time1 = "2020-05-17 07:39:22",
                                    time2 = "2020-05-17 04:31:36")),
                         tolerance = 0.01)

})

#################
# Log Durations #
#################

test_that("daily log durations completes the time series with 0 seconds", {
  log_file_path <- "./testdata/test_multiple_files/"
  actual_in <- tibble::tibble(log_file_name = c("log-20200409-124015.txt",
                                                "log-20200526-124016.txt"))

  logs_df <-
    read_fah_logs(actual_in, log_file_path) %>%
    clean_logs()
  actual = get_daily_duration(logs_df)
  testthat::expect_length(unique(actual$log_date), 48)
})
