library(fahlogstats)
library(dplyr)
library(ggplot2)

logs_path <-  "~/../AppData/Roaming/FAHClient/logs/old_logs/"

# This logic at the start should backup the current "active" folder to "old_logs"
# and then read from the full set of files in the "old_logs" folder.

# Backup Logs
if(!isTRUE(file.info("~/../AppData/Roaming/FAHClient/logs/old_logs")$isdir))
  dir.create(paste0(logs_path, backup_folder_name))

file.copy(paste0("~/../AppData/Roaming/FAHClient/logs/",
                 list.files(pattern = "*.txt", path = "~/../AppData/Roaming/FAHClient/logs/")),
          paste0("~/../AppData/Roaming/FAHClient/logs/", "old_logs",  "/",
                 list.files(pattern = "*.txt", path = "~/../AppData/Roaming/FAHClient/logs/")))

logs_df <-
  fahlogstats::read_fah_logs_dir(logs_path) %>%
  clean_logs()

# live logs

live_logs_df <-
  fahlogstats::read_live_log("~/../AppData/Roaming/FAHClient/") %>%
  fahlogstats::clean_logs() %>%
  dplyr::union_all(logs_df)


live_logs_df %>%
  get_work_unit_data() %>%
  get_credits() %>%
  plot_credits(all_slots = TRUE)

live_logs_df %>%
  get_work_unit_data() %>%
  get_network_usage() %>%
  calculate_daily_network_usage() %>%
  plot_cumulative_network_usage()

# Ip addresses
live_logs_df %>%
  get_work_unit_data() %>%
  get_connections_data() %>%
  dplyr::pull(ip_address) %>%
  sample(20) %>%
  purrr::map_df(get_from_ip_api) %>%
  group_by(isp,org) %>%
  tally() %>%
  arrange(n)

plot_weekly_idle_slots(live_logs_df)

total_duration <-
  live_logs_df %>%
  get_daily_duration() %>%
  mutate(total_log_duration = as.difftime(as.integer(total_log_duration) / 3600, units = "hours")) %>%
  ungroup() %>%
  summarise(total_hours = sum(total_log_duration))

idle_summary <-
  live_logs_df %>%
  get_processing_time_summary()%>%
  group_by(folding_slot) %>%
  summarise(total_active_hours = sum(total_processing_time)) %>%
  mutate(total_log_hours = total_duration$total_hours,
         total_idle_hours = total_log_hours - total_active_hours)



idle_summary %>%
  dplyr::ungroup() %>%
  summarise(idle_percentage =
              100 * (
                sum(as.numeric(total_idle_hours)) /
                  sum(as.numeric(total_log_hours))
                )
            )



idle_per_slot_summary <-
  live_logs_df %>%
  get_processing_time_summary() %>%
  group_by(log_date, folding_slot) %>%
  summarise(daily_processing_time = sum(total_processing_time)) %>%
  dplyr::left_join(get_daily_duration(live_logs_df),
                   by = "log_date") %>%
  mutate(total_log_duration = as.difftime(as.integer(total_log_duration) / 3600, units = "hours"),
         daily_idle_time = total_log_duration - daily_processing_time)

idle_per_slot_summary %>%
tidyr::complete(folding_slot, log_date, nesting(total_log_duration))

idle_per_slot_summary %>%
  group_by(log_date, folding_slot) %>%
  tidyr::complete(log_date, folding_slot, fill = list(daily_processing_time = NA,
                                                      log_duration = NA,
                                                      daily_idle_time = NA)) %>%
  left_join(idle_per_slot_summary, by = c("log_date", "folding_slot")) %>%
  arrange(log_date)
  group_by(log_date) %>%
  summarise(total_daily_idle_time = sum(daily_idle_time),
            num_cores = n_distinct(folding_slot)) %>%
  ungroup() %>%
  summarise(mean_daily_idle_time = mean(total_daily_idle_time))


# live data

live_logs_df %>%
  get_folding_slot_names()
