#' Ingest Data
#'
#' Gather data from logs and other sources.\cr\cr
#' **read_fah_logs:** Takes a path to a directory of FaH Client log stats and returns
#' the logs parsed into a tibble (data frame). The output data frame
#' is quite sparse and will include data in many different formats,
#' i.e. there are lots of columns, none of them named, and lots of
#' NA's. We must further split/clean the data to get tidy data.
#' @name read_fah_logs
NULL



#'
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


#' Read a single log.
read_fah_logs <- function(logs_path) {
  logs <-
    tibble::tibble(log_file_name = list.files(pattern = "*.txt", path = logs_path)) %>%
    dplyr::mutate(log_df = map(log_file_name, read_log, logs_path))
}

read_log <- function(log_file_name, path) {
  file_path <- paste0(path, log_file_name)

  log_df <- tibble::tibble(message = scan(file_path,
                                  what = "character",
                                  sep = "\r")) %>%
    filter(!stringr::str_starts(message, "\\*"))
  log_df
}


clean_logs <- function(logs_df) {
  parsed_log <-
    logs_df %>%
    mutate(log_date = map_chr(log_file_name,
                              function(x) str_extract(x, "\\d+")),
           log_date = as.Date(log_date, format = "%Y%m%d")) %>%
    unnest(log_df)


  parsed_log <-
    parsed_log %>%
    mutate(log_time = str_sub(message, 1, 8),
           message = str_sub(message, 10, 10000),
           message = str_trim(message),
           log_timestamp = lubridate::ymd_hms(paste(log_date, log_time)))

  tidyr::separate(parsed_log,
                  col = message,
                  into = as.character(1:13),
                  sep = ":")
}


# IP Address Lookup

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
