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

read_fah_logs <- function(logs_path) {
  # TODO: Check if the path is a folder or a file and return data accordingly.
  tibble::tibble(log_file_name = list.files(pattern = "*.txt",
                                            path = logs_path)) %>%
    dplyr::mutate(log_df = purrr::map(log_file_name, read_log, logs_path))
}


read_log <- function(log_file_name, path) {
  # Read a single log.
  file_path <- paste0(path, log_file_name)

  log_df <- tibble::tibble(message = scan(file_path,
                                          what = "character",
                                          sep = "\r"))
  log_df <- dplyr::filter(log_df, !stringr::str_starts(message, "\\*"))

  log_df
}


#' @rdname read_fah_logs
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
    dplyr::mutate(log_date = purrr::map_chr(log_file_name,
                                            function(x) stringr::str_extract(x, "\\d+")),
                  log_date = as.Date(log_date, format = "%Y%m%d")) %>%
    tidyr::unnest(log_df)

  # TODO: you can't rely on the log date for the date information,
  # if the log rolls over to a new day you must increment the date
  # or read it from somewhere in the log data itself.
  parsed_log <-
    parsed_log %>%
    dplyr::mutate(log_time = stringr::str_sub(message, 1, 8),
                  message = stringr::str_sub(message, 10, 10000),
                  message = stringr::str_trim(message),
                  log_timestamp = lubridate::ymd_hms(paste(log_date, log_time)))

  tidyr::separate(parsed_log,
                  col = message,
                  into = as.character(1:13),
                  sep = ":")
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
