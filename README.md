# fahlogstats
R library for analysing Folding@Home client log data.
Sister library to the Shiny dashboard [(fahdashboard)](https://github.com/cormac85/fahdashboard) that intends
to provide insights about your folding slots, such as network data usage and credits earned.

## Installation
```R
devtools::install_github("cormac85/fahlogstats")
```
## Usage
```R
logs_path <-  "~/../AppData/Roaming/FAHClient/logs/"

logs_df <-
  read_fah_logs_dir(logs_path) %>%
  clean_logs()
  
 
logs_df %>%
  get_work_unit_data() %>%
  get_credits() %>%
  plot_credits(all_slots = TRUE)
  
logs_df %>%
  get_work_unit_data() %>%
  get_network_usage() %>%
  calculate_daily_network_usage() %>%
  plot_cumulative_network_usage()
```

## Background
On Windows the Folding@Home client stores the current log that you see in the Advanced Client
in `~/AppData/Roaming/FAHClient/log.txt`, and also stores 16 days worth of logs in `~/AppData/Roaming/FAHClient/logs/`.
This library is intended to parse those logs into various types of tidy dataframes and then provide subsequent 
analysis with plots and summaries.


