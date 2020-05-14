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

