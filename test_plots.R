library(fahlogstats)
library(dplyr)
library(ggplot2)

logs_path <-  "~/../AppData/Roaming/FAHClient/logs/old_logs/"

logs_df <-
  fahlogstats::read_fah_logs(logs_path) %>%
  clean_logs()

summary(logs_df)

logs_df %>%
  get_work_unit_data() %>%
  get_credits() %>%
  plot_credits()
