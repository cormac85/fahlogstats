# Add time columns
add_processing_time_cols <- function(parsed_log) {
  processing_time_df <-
    parsed_log %>%
    filter(str_detect(`3`, "^0x"),
           str_detect(`4`, "Completed")) %>%
    rename(work_unit = `1`,
           folding_slot = `2`,
           core = `3`,
           progress_message = `4`) %>%
    select(log_file_name, log_timestamp, log_date,
           log_time, folding_slot, work_unit,
           core, progress_message) %>%
    arrange(folding_slot, work_unit, log_timestamp)

  processing_time_df <-
    processing_time_df %>%
    mutate(end_flag = str_detect(progress_message, "100\\%\\)")) %>%
    group_by(folding_slot, work_unit) %>%
    mutate(work_id = cumsum(end_flag),
           work_id = ifelse(end_flag, work_id - 1, work_id),
           work_id = paste(folding_slot, work_unit, work_id, sep = "-")) %>%
    group_by(work_id) %>%
    mutate(previous_step_timestamp = lag(log_timestamp, 1),
           step_time_diff = log_timestamp - previous_step_timestamp,
           step_time_diff = ifelse(step_time_diff > 1000, 0, step_time_diff))

  processing_time_df
}

get_processing_time_summary <- function(parsed_log) {
  processing_time_summary <-
    parsed_log %>%
    add_processing_time_cols() %>%
    group_by(folding_slot, work_unit, work_id) %>%
    summarise(total_processing_time = sum(step_time_diff, na.rm = TRUE) / 3600)

  processing_time_summary
}

get_total_log_duration <- function(parsed_log) {
  parsed_log %>%
    group_by(log_file_name) %>%
    summarise(start_log = min(log_timestamp),
              end_log = max(log_timestamp)) %>%
    mutate(log_duration = (end_log - start_log)) %>%
    ungroup() %>%
    summarise(total_log_duration = sum(log_duration)) %>%
    pull(total_log_duration) %>%
    (function(x) as.integer(x) / 3600)
}

# processing_time_summary <- get_processing_time_summary(parsed_log_expanded)
# total_log_duration <- get_total_log_duration(parsed_log_expanded)
# total_usage_gib <- sum(network_usage_daily_summary$total_usage_mib) / 1024



# Credits
get_credits <- function(log_df) {
  log_df %>%
    filter(str_detect(`3`, "Final")) %>%
    rename(credits_attributed = `3`) %>%
    select(work_unit, folding_slot, credits_attributed,
           log_time, log_date, log_timestamp) %>%
    mutate(credits_attributed = as.numeric(
      str_extract(credits_attributed, "\\d+")
    ))
}


# Network usage
get_network_usage <- function(work_units_df) {
  work_units_df %>%
    filter(str_detect(`3`, "Downloading")) %>%
    rename(usage_mib = `3`) %>%
    select(log_file_name, folding_slot, work_unit,
           log_timestamp, log_date, log_time,
           usage_mib) %>%
    mutate(usage_mib = str_extract(usage_mib, "(\\d+).(\\d+)"),
           network_direction = "download") %>%
    union_all(
      work_units_df %>%
        filter(str_detect(`3`, "Uploading")) %>%
        rename(usage_mib = `3`) %>%
        select(log_file_name, folding_slot, work_unit,
               log_timestamp, log_date, log_time,
               usage_mib) %>%
        mutate(usage_mib = str_extract(usage_mib, "(\\d+).(\\d+)"),
               network_direction = "upload")
    ) %>%
    mutate(usage_mib = as.numeric(usage_mib))
}

calculate_daily_network_usage <- function(network_usage_df) {
  network_usage_df %>%
    group_by(log_date, folding_slot, network_direction) %>%
    summarise(total_usage_mib = sum(usage_mib)) %>%
    ungroup() %>%
    complete(folding_slot,
             network_direction,
             log_date = seq.Date(min(log_date), max(log_date), by = "day"),
             fill = list(total_usage_mib = 0)) %>%
    arrange(log_date)
}


# IP Addresses
# connections_df <-
#   log_work_units_df %>%
#   filter(str_detect(`3`, "Connecting")) %>%
#   rename(ip_address = `3`) %>%
#   select(log_file_name, log_timestamp, log_date,
#          log_time, folding_slot, work_unit,
#          ip_address) %>%
#   mutate(ip_address = str_extract(ip_address, "(\\d+).(\\d+).(\\d+).(\\d+)"))
#
#
# ip_address_df <- tibble(ip_address = unique(connections_df$ip_address))
