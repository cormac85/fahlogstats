# Process & Enrich Data

#' Clean FAH Log Data
#'
#' Cleans the output of `read_fah_logs()`
#'
#' @param logs_df The log files tibble as read in by read_fah_logs()
#'
#' @return **clean_logs:** Tibble of cleaned log data.
#'
#' @export
#' @examples
#' # Windows 10:
#' my_log_data <- read_fah_logs("~/../AppData/Roaming/FAHClient/logs/")
#' my_clean_log_data <- clean_logs(my_log_data)
#' summary(my_clean_log_data)

clean_logs <- function(logs_df) {
  parsed_log <-
    logs_df %>%
    dplyr::mutate(log_row_index = 1:nrow(logs_df)) %>%
    dplyr::mutate(log_time = stringr::str_sub(message, 1, 8),
                  message = stringr::str_sub(message, 10, 10000),
                  message = stringr::str_trim(message),
                  log_timestamp = lubridate::ymd_hms(paste(log_date, log_time)))

  parsed_log <-
    parsed_log %>%
    dplyr::arrange(log_row_index) %>%
    dplyr::group_by(log_file_name) %>%
    dplyr::mutate(lagged_time_diff = log_timestamp - dplyr::lag(log_timestamp),
                  is_date_rollover = ifelse(lagged_time_diff >=0, FALSE, TRUE))
  parsed_log <-
    parsed_log %>%
    dplyr::mutate(is_date_rollover = cumsum( dplyr::coalesce(is_date_rollover, FALSE)),
                  log_timestamp = log_timestamp + lubridate::days(is_date_rollover),
                  log_date = log_date + lubridate::days(is_date_rollover)) %>%
    dplyr::ungroup()


  suppressWarnings(tidyr::separate(parsed_log,
                                   col = message,
                                   into = as.character(1:13),
                                   sep = ":",
                                   fill = "right")
  )
}


#' Get Processing Time Summary
#'
#' @param parsed_log A tibble of FAH Client logs that are parsed
#' (`read_fah_logs()`) and cleaned (`clean_logs()`).
#'
#' @return Tibble summary of processing time for different work units by
#' folding slot.
#'
#' @export
#' @examples
#' read_fah_logs("~/../AppData/Roaming/FAHClient/logs/") %>%
#'   clean_logs() %>%
#'   get_processing_time_summary()

get_processing_time_summary <- function(parsed_log) {

  parsed_log %>%
    add_processing_time_cols() %>%
    dplyr::group_by(folding_slot, work_unit, work_id, log_date) %>%
    dplyr::summarise(
      total_processing_time = sum(step_time_diff, na.rm = TRUE) / 3600
    )
}

add_processing_time_cols <- function(parsed_log) {
  # Adds columns needed to calculate the length of
  # time each work unit takes.
  processing_time_df <-
    parsed_log %>%
    dplyr::filter(stringr::str_detect(`3`, "^0x"),
                  stringr::str_detect(`4`, "Completed")) %>%
    dplyr::rename(work_unit = `1`,
                  folding_slot = `2`,
                  core = `3`,
                  progress_message = `4`) %>%
    dplyr::select(log_file_name, log_timestamp, log_date,
                  log_time, folding_slot, work_unit,
                  core, progress_message) %>%
    dplyr::mutate(folding_slot = stringr::str_sub(folding_slot, 3)) %>%
    dplyr::arrange(folding_slot, work_unit, log_timestamp)

  processing_time_df <-
    processing_time_df %>%
    dplyr::mutate(
      end_flag = stringr::str_detect(progress_message, "100\\%\\)")
    ) %>%
    dplyr::group_by(folding_slot, work_unit) %>%
    dplyr::mutate(work_id = cumsum(end_flag),
                  work_id = ifelse(end_flag, work_id - 1, work_id),
                  work_id = paste(folding_slot,
                                  work_unit,
                                  work_id,
                                  sep = "-")) %>%
    dplyr::group_by(work_id) %>%
    dplyr::mutate(previous_step_timestamp = dplyr::lag(log_timestamp, 1),
                  step_time_diff = log_timestamp - previous_step_timestamp,
                  step_time_diff = ifelse(step_time_diff > 1000,
                                          0,
                                          step_time_diff
                  ))

  processing_time_df
}

#' Get Work Unit Data
#'
#' @export
#'
get_work_unit_data <- function(parsed_log){
  parsed_log %>%
    dplyr::filter(stringr::str_detect(`2`, "FS\\d+")) %>%
    dplyr::rename(work_unit = `1`,
                  folding_slot = `2`) %>%
    dplyr::mutate(folding_slot = stringr::str_sub(folding_slot, 3))
}

#' Get Debug Data
#'
#' @export
#'
get_debug_data <- function(parsed_log){
  parsed_log %>%
    dplyr::filter(`1` == "WARNING" | `1` == "ERROR") %>%
    dplyr::rename(debug_level = `1`,
                  work_unit = `2`,
                  folding_slot = `3`)
}

#' Get Daily Duration
#'
#' Calculates the total length of time (in hours) that the FAH Client
#' was active per day.
#'
#' @param parsed_log A tibble of FAH Client logs that are parsed
#' (`read_fah_logs()`) and cleaned (`clean_logs()`).
#'
#' @return A dataframe representing the total active log hours per day,
#' i.e. how long the FAH Client was active, logging and could have been
#' folding if work was available.
#'
#' @export
#' @examples
#' read_fah_logs("~/../AppData/Roaming/FAHClient/logs/") %>%
#'   clean_logs() %>%
#'   get_total_log_duration() %>%
#'   paste("Total time client open:", ., "hours")
get_daily_duration <- function(parsed_log) {
  daily_duration <-
    parsed_log %>%
    dplyr::group_by(log_file_name, log_date) %>%
    dplyr::summarise(start_log = min(log_timestamp),
                     end_log = max(log_timestamp)) %>%
    dplyr::mutate(log_duration = (end_log - start_log)) %>%
    dplyr::group_by(log_date) %>%
    dplyr::summarise(total_log_duration = sum(log_duration))

  daily_duration %>%
    dplyr::ungroup() %>%
    tidyr::complete(total_log_duration,
                    log_date = seq.Date(min(log_date), max(log_date), by = "day"),
                    fill = list(total_log_duration = 0))
}

# daily_network_usage %>%
#   dplyr::ungroup() %>%
#   tidyr::complete(folding_slot,
#                   network_direction,
#                   log_date = seq.Date(min(log_date), max(log_date), by = "day"),
#                   fill = list(total_usage_mib = 0))


#' Get Credits
#' @export
get_credits <- function(work_units_df) {
  work_units_df %>%
    dplyr::filter(stringr::str_detect(`3`, "Final")) %>%
    dplyr::rename(credits_attributed = `3`) %>%
    dplyr::select(work_unit, folding_slot, credits_attributed,
                  log_time, log_date, log_timestamp) %>%
    dplyr::mutate(credits_attributed = as.numeric(
      stringr::str_extract(credits_attributed, "\\d+")
    ))
}


#' Credit Summary
#' @export
get_credit_summary <- function(credits_df, all_slots = FALSE) {
  if(all_slots) {
    credits_df <- dplyr:: mutate(credits_df, folding_slot = "all")
  }

  credits_per_day <-
    credits_df %>%
    dplyr::group_by(folding_slot, log_date) %>%
    dplyr::summarise(sum_daily = sum(credits_attributed))

  credits_per_work_unit <-
    credits_df %>%
    dplyr::group_by(folding_slot) %>%
    dplyr::summarise(credits_per_wu = mean(credits_attributed))

  credits_per_slot <-
    credits_per_day %>%
    dplyr::group_by(folding_slot) %>%
    dplyr::summarise(total_credits_attributed = sum(sum_daily),
                     mean_credits_attributed = mean(sum_daily)) %>%
    dplyr::mutate(x = min(credits_df$log_date),
                  y = max(credits_per_day$sum_daily)) %>%
    dplyr::left_join(credits_per_work_unit, by = "folding_slot")

  credits_per_slot

}

#' Get Network Usage
#'
#' @export
get_network_usage <- function(work_units_df) {

  upload_df <-
    work_units_df %>%
    dplyr::filter(stringr::str_detect(`3`, "Downloading")) %>%
    dplyr::rename(usage_string = `3`) %>%
    dplyr::select(log_file_name, folding_slot, work_unit,
                  log_timestamp, log_date, log_time,
                  usage_string) %>%
    dplyr::mutate(usage_value = stringr::str_extract(usage_string,
                                                     "(\\d+).(\\d+)"),
                  usage_unit = stringr::str_extract(usage_string, "(\\d+).(\\d+)\\w"),
                  usage_unit = stringr::str_trunc(usage_unit,
                                                  width = 1,
                                                  side = "left",
                                                  ellipsis = ""),
                  network_direction = "download")

  download_df <-
      work_units_df %>%
        dplyr::filter(stringr::str_detect(`3`, "Uploading")) %>%
        dplyr::rename(usage_string = `3`) %>%
        dplyr::select(log_file_name, folding_slot, work_unit,
                      log_timestamp, log_date, log_time,
                      usage_string) %>%
        dplyr::mutate(usage_value = stringr::str_extract(usage_string,
                                                       "(\\d+).(\\d+)"),
                      usage_unit = stringr::str_extract(usage_string,
                                                        "(\\d+).(\\d+)\\w"),
                      usage_unit = stringr::str_trunc(usage_unit,
                                                      width = 1,
                                                      side = "left",
                                                      ellipsis = ""),
                      network_direction = "upload")


  usage_df <-
    upload_df %>%
    dplyr::union_all(download_df) %>%
    dplyr::mutate(usage_value = as.numeric(usage_value)) %>%
    dplyr::select(-usage_string)

  usage_df %>%
    dplyr::mutate(usage_mib = dplyr::case_when(
      usage_unit == "K" ~ usage_value / 1000,
      usage_unit == "G" ~ usage_value * 1000,
      usage_unit == "M" ~ usage_value,
      TRUE ~ NA_real_
    )) %>%
    dplyr::select(-usage_unit, -usage_value)
}


#' Calculate Daily Network Usage
#'
#' @export

calculate_daily_network_usage <- function(network_usage_df) {
  daily_network_usage <-
    network_usage_df %>%
    dplyr::group_by(log_date, folding_slot, network_direction) %>%
    dplyr::summarise(total_usage_mib = sum(usage_mib))

  daily_network_usage %>%
    dplyr::ungroup() %>%
    tidyr::complete(folding_slot,
                    network_direction,
                    log_date = seq.Date(min(log_date), max(log_date), by = "day"),
                    fill = list(total_usage_mib = 0)) %>%
    dplyr::arrange(log_date)
}


#' Get Connections Data
#'
#' @export
get_connections_data <- function(work_units_df) {
  work_units_df %>%
    dplyr::filter(stringr::str_detect(`3`, "Connecting")) %>%
    dplyr::rename(ip_address = `3`) %>%
    dplyr::select(log_file_name, log_timestamp, log_date,
                  log_time, folding_slot, work_unit,
                  ip_address) %>%
    dplyr::mutate(ip_address = stringr::str_extract(ip_address, "(\\d+).(\\d+).(\\d+).(\\d+)"))
}


#' Get Folding Slot Names
#'
#' @export
get_folding_slot_names <- function(logs_df) {
  gpu_names <-
    logs_df  %>%
    dplyr::filter(stringr::str_detect(`1`, "GPU \\d")) %>%
    dplyr::rename(gpu_name = `6`, gpu_number = `1`) %>%
    dplyr::mutate(gpu_name = stringr::str_sub(gpu_name, start = 3)) %>%
    dplyr::select(log_file_name, gpu_number, gpu_name, log_date:is_date_rollover) %>%
    dplyr::distinct(gpu_number, gpu_name) %>%
    dplyr::mutate(gpu_number = stringr::str_extract(gpu_number, "\\d"))

  work_units_df <- get_work_unit_data(logs_df)

  gpu_slots <-
    logs_df %>%
    dplyr::filter(stringr::str_detect(`2`, "READY gpu")) %>%
    dplyr::rename(gpu_name = `2`, gpu_number = `3`, folding_slot = `1`) %>%
    dplyr::mutate(folding_slot = stringr::str_extract(folding_slot, "\\d+")) %>%
    dplyr::distinct(folding_slot, gpu_number) %>%
    dplyr::left_join(gpu_names, by = "gpu_number")

  cpu_slots <-
    work_units_df  %>%
    dplyr::filter(trimws(`4`) == "CPU") %>%
    dplyr::rename(cpu_name = `5`) %>%
    dplyr::distinct(folding_slot, cpu_name) %>%
    dplyr::mutate(cpu_name = trimws(cpu_name)) %>%
    dplyr::filter(! stringr::str_detect(cpu_name, "0x00000000000000000000000000000000"))

  cpu_slots %>%
    dplyr::left_join(gpu_slots, by = "folding_slot") %>%
    dplyr::mutate(processor_name = ifelse(is.na(gpu_name), cpu_name, gpu_name),
                  processor_type = ifelse(is.na(gpu_name), "CPU", "GPU"))
}


#' Get Live Folding Slot Progress
#'
#' @export
get_slot_progress <- function(logs_df) {

  current_work_start_time <-
    logs_df %>%
    get_work_unit_data() %>%
    dplyr::filter(stringr::str_detect(`3`, "Started FahCore on PID")) %>%
    dplyr::group_by(folding_slot) %>%
    dplyr::summarise(work_start = dplyr::last(log_timestamp))

  slot_progress <-  suppressWarnings(
    logs_df %>%
      get_work_unit_data() %>%
      dplyr::filter(stringr::str_detect(`4`, "Completed")) %>%
      dplyr::select(-(`5`:`13`)) %>%
      dplyr::rename("core" = `3`,
             "completed_status" = `4`) %>%
      tidyr::separate(col = completed_status, into = c("completed_steps", "completed_percent"), sep = "[\\(\\%\\)]") %>%
      dplyr::arrange(log_timestamp) %>%
      dplyr::group_by(folding_slot) %>%
      dplyr::summarise(slot_progress = as.numeric(dplyr::last(completed_percent)),
                       progress_timestamp = dplyr::last(log_timestamp))
  )

  slot_progress <-
    logs_df %>%
    get_folding_slot_names() %>%
    dplyr::left_join(
      slot_progress, by = "folding_slot"
    ) %>%
    dplyr::left_join(
      current_work_start_time, by = "folding_slot"
    )

  slot_progress
}

#' Get Latest Complete Work Unit Details
#'
#' @export
get_latest_work_unit_details <- function(work_units_df) {

  latest_work_start_time <-
    work_units_df %>%
    dplyr::filter(stringr::str_detect(`3`, "Started FahCore on PID")) %>%
    dplyr::group_by(folding_slot, work_unit) %>%
    dplyr::summarise(work_start = dplyr::last(log_timestamp))


  latest_work_end_time <-
    work_units_df %>%
    dplyr::filter(stringr::str_detect(`3`, "Final")) %>%
    dplyr::rename(credits_attributed = `3`) %>%
    dplyr::select(work_unit, folding_slot, credits_attributed,
                  log_time, log_date, log_timestamp) %>%
    dplyr::group_by(folding_slot, work_unit) %>%
    dplyr::summarise(
      latest_credits_attributed = dplyr::last(as.numeric(
        stringr::str_extract(credits_attributed, "\\d+")
        )),
      work_end = dplyr::last(log_timestamp)
      )

  completed_work_unit_details <-
    latest_work_start_time %>%
    dplyr::inner_join(
      latest_work_end_time,
      by = c("folding_slot", "work_unit")
    ) %>%
    dplyr::mutate(latest_work_duration = difftime(work_end,work_start,
                                                  units = "hours")) %>%
    dplyr::filter(latest_work_duration > 0) %>%
    dplyr::select(folding_slot,
                  work_unit,
                  work_start,
                  work_end,
                  latest_work_duration,
                  latest_credits_attributed)

  completed_work_unit_details %>%
    dplyr::group_by(folding_slot) %>%
    dplyr::filter(work_end == max(work_end))

}
