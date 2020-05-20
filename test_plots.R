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

live_logs_df %>%
  get_processing_time_summary() %>%
  dplyr::mutate(week_number = strftime(log_date, format = "%Y-%W")) %>%
  group_by(folding_slot, week_number) %>%
  summarise(total_processing_time = sum(total_processing_time)) %>%
  left_join(
    live_logs_df %>%
      get_daily_duration() %>%
      dplyr::mutate(week_number = strftime(log_date, format = "%Y-%W")) %>%
      group_by(week_number) %>%
      summarise(total_log_duration = sum(total_log_duration)),
    by = "week_number") %>%
  mutate(utilisation_percent = total_processing_time / (as.numeric(total_log_duration) / 3600),
         idle_percent = 1 - utilisation_percent) %>%
  tidyr::pivot_longer(cols = c(utilisation_percent, idle_percent), names_to = "metric", values_to = "value") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(folding_slot = paste0("Folding Slot ", as.numeric(folding_slot))) %>%
  ggplot2::ggplot(ggplot2::aes(week_number, value, fill = metric, group = metric)) +
  geom_col() +
  facet_wrap(~folding_slot, ncol = 1, ) +
  scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_minimal(base_size = BASE_PLOT_TEXT_SIZE) +
  ggplot2::theme(legend.position = "top",
                 axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2))) +
  ggplot2::labs(title = paste0("Utilisation per Day"),
                subtitle = paste0(min(live_logs_df$log_date), " - ",
                                  max(live_logs_df$log_date)),
                x = "Week Number", y = "Utilisation vs Idle Time (%)",
                fill = "Idleness", caption = "Folding Slot") +
  ggplot2::scale_fill_manual(values = fah_web_palette,
                             labels = c("Idle Percent", "Utilisation Percent"))

