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
    dplyr::mutate(log_date =
                    purrr::map_chr(log_file_name,
                                   function(x) stringr::str_extract(x, "\\d+")),
                  log_date = as.Date(log_date, format = "%Y%m%d")) %>%
    tidyr::unnest(log_df)

  # TODO: you can't rely on the log date for the date information,
  # if the log rolls over to a new day you must increment the date
  # or read it from somewhere in the log data itself.
  parsed_log <-
    parsed_log %>%
    dplyr::mutate(log_row_index = 1:nrow(parsed_log)) %>%
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
                  log_date = log_date + lubridate::days(is_date_rollover))


  tidyr::separate(parsed_log,
                  col = message,
                  into = as.character(1:13),
                  sep = ":")
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
    dplyr::group_by(folding_slot, work_unit, work_id) %>%
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
                  folding_slot = `2`)
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

add_cumulative_sum <- function(log_df, sum_column, date_column, ...) {
  # adds a cumulative sum column based a given column and group keys
  log_df %>%
    dplyr::arrange(folding_slot, ..., {{date_column}}) %>%
    dplyr::group_by(folding_slot, ...) %>%
    dplyr::mutate("cumulative_{{ sum_column }}" := cumsum({{ sum_column }}))
}

#' Get Total Log Duration
#'
#' Calculates the total length of time (in hours) that the FAH Client
#' was active.
#'
#' @param parsed_log A tibble of FAH Client logs that are parsed
#' (`read_fah_logs()`) and cleaned (`clean_logs()`).
#'
#' @return A double representing the total hours that logging took place,
#' i.e. how long the FAH Client was active, logging and could have been
#' folding if work was available.
#'
#' @export
#' @examples
#' read_fah_logs("~/../AppData/Roaming/FAHClient/logs/") %>%
#'   clean_logs() %>%
#'   get_total_log_duration() %>%
#'   paste("Total time client open:", ., "hours")
get_total_log_duration <- function(parsed_log) {
  parsed_log %>%
    dplyr::group_by(log_file_name) %>%
    dplyr::summarise(start_log = min(log_timestamp),
                     end_log = max(log_timestamp)) %>%
    dplyr::mutate(log_duration = (end_log - start_log)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(total_log_duration = sum(log_duration)) %>%
    dplyr::pull(total_log_duration) %>%
    (function(x) as.integer(x) / 3600)
}


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


#' Get Network Usage
#'
#' @export
get_network_usage <- function(work_units_df) {
  work_units_df %>%
    dplyr::filter(stringr::str_detect(`3`, "Downloading")) %>%
    dplyr::rename(usage_mib = `3`) %>%
    dplyr::select(log_file_name, folding_slot, work_unit,
                  log_timestamp, log_date, log_time,
                  usage_mib) %>%
    dplyr::mutate(usage_mib = stringr::str_extract(usage_mib, "(\\d+).(\\d+)"),
                  network_direction = "download") %>%
    dplyr::union_all(
      work_units_df %>%
        dplyr::filter(stringr::str_detect(`3`, "Uploading")) %>%
        dplyr::rename(usage_mib = `3`) %>%
        dplyr::select(log_file_name, folding_slot, work_unit,
                      log_timestamp, log_date, log_time,
                      usage_mib) %>%
        dplyr::mutate(usage_mib = stringr::str_extract(usage_mib,
                                                       "(\\d+).(\\d+)"),
                      network_direction = "upload")
    ) %>%
    dplyr::mutate(usage_mib = as.numeric(usage_mib))
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

