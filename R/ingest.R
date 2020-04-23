#' Ingest Data
#'
#' Gather data from logs and other sources.\cr\cr
#' **read_fah_logs:** Takes a path to a directory of FaH Client log
#' stats and returns the logs parsed into a tibble (data frame). \cr\cr
#' **clean_logs:** Takes the output of `read_fah_logs()` and returns a
#' cleaner tibble with dates and time types created.
#' The output data frame is quite sparse and will
#' include data in many different formats,i.e. there are lots of
#' columns, none of them named, and lots of NA's. We must further
#' split/clean the data to get tidy data.
#' @name read_fah_logs
NULL

#' @rdname read_fah_logs
#' @param logs_path A path string to the FaH Client logs.
#'
#' @return **read_fah_logs:** Tibble of parsed log data.
#'
#' @export
#' @examples
#' # Windows 10:
#' my_log_data <- read_fah_logs("~/../AppData/Roaming/FAHClient/logs/")
#' summary(my_log_data)

read_fah_logs <- function(logs_path_df, logs_path) {
  logs_path_df %>%
    dplyr::mutate(log_df = purrr::map(log_file_name, read_log, logs_path)) %>%
    dplyr::mutate(log_date =
                    purrr::map_chr(log_file_name,
                                   function(x) stringr::str_extract(x, "\\d+")),
                  log_date = as.Date(log_date, format = "%Y%m%d")) %>%
    tidyr::unnest(log_df)
}

#' Read FAH Logs from a Directory
#' @export
read_fah_logs_dir <- function(logs_path, log_name_pattern = "*.txt"){
  tibble::tibble(log_file_name = list.files(pattern = log_name_pattern,
                                            path = logs_path)) %>%
    read_fah_logs(logs_path)
}

#' Read live log
#' @export
read_live_log <- function(fah_client_path) {
  raw_log_df <- read_fah_logs_dir(fah_client_path, "log.txt")

  log_start_date <-
    log_df %>%
    dplyr::filter(stringr::str_detect(message, " Log Started")) %>%
    (function(x) x$message[1]) %>%
    stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    lubridate::ymd()

  raw_log_df$log_date <- log_start_date

  raw_log_df
}

read_log <- function(log_file_name, path) {
  # Read a single log.
  file_path <- paste0(path, log_file_name)

  log_df <- suppressMessages(tibble::tibble(message = scan(file_path,
                                                           what = "character",
                                                           sep = "\r",
                                                           quiet = TRUE)))
  log_df <- dplyr::filter(log_df, !stringr::str_starts(message, "\\*"))

  log_df
}


#' Get IP Data From IP API
#'
#' API: http://ip-api.com/json/
#'
#' @export

get_from_ip_api <- function(ip_addr) {
  api_url <- "http://ip-api.com/json/"
  api_query <- paste0(api_url, ip_addr)
  httr::GET(api_query) %>%
    httr::content() %>%
    unlist() %>%
    tibble::enframe() %>%
    tidyr::pivot_wider() %>%
    janitor::clean_names()
}
