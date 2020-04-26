#' Plot Credits by Folding Slot
#'
#' Given the output of `get_credits()`, this will create a stacked bar chart of
#' credits awarded by folding slot. Set `all_slots = TRUE` if you want to see
#' total credits of all folding slots.
#'
#' @param parsed_log A tibble of FAH Client logs that are parsed
#' (`read_fah_logs()`) and cleaned (`clean_logs()`).
#'
#' @return A plot of credits acquired per day.
#'
#' @export
#' @examples
#' read_fah_logs("~/../AppData/Roaming/FAHClient/logs/") %>%
#'   clean_logs() %>%
#'   get_credits() %>%
#'   plt_credits()
plot_credits <- function(credits_df, all_slots = FALSE) {

  credits_plot <-
    credits_df %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(log_date,
                                   credits_attributed,
                                   fill = folding_slot),
                      position = "stack", colour = fah_web_palette[5]) +

    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%a %F") +
    ggplot2::theme(legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2))) +
    ggplot2::labs(title = paste0("Credits Acquired Per Day"),
                  subtitle = paste0(min(credits_df$log_date), " - ",
                                    max(credits_df$log_date)),
                  x = "Date", y = "Credits",
                  fill = "Folding Slot") +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = fah_web_palette)

  credits_per_slot <- get_credit_summary(credits_df, all_slots = all_slots)

  if(all_slots){
    credits_plot <-
      credits_plot +
      ggplot2::geom_label(data = credits_per_slot,
                          inherit.aes = FALSE,
                          ggplot2::aes(x = x, y = y - (0.1 * y),
                                       label = paste(
                                         "Total Credits: ",
                                         scales::comma(total_credits_attributed)
                                       )),
                          fill = fah_web_palette[2], colour = "white",
                          fontface = "bold", hjust = 0.1) +
      ggplot2::geom_label(data = credits_per_slot,
                          inherit.aes = FALSE,
                          ggplot2::aes(x = x, y = y - (0.2 * y),
                                       label = paste(
                                         "Credits per Work Unit: ",
                                         scales::comma(credits_per_wu)
                                       )),
                          fill = fah_web_palette[2], colour = "white",
                          fontface = "bold", hjust = 0.1) +
      ggplot2::geom_label(data = credits_per_slot,
                          inherit.aes = FALSE,
                          ggplot2::aes(x = x, y = y - (0.3 * y),
                                       label = paste(
                                         "Credits Per Day: ",
                                         scales::comma(mean_credits_attributed)
                                       )),
                          fill = fah_web_palette[2],
                          colour = "white",
                          fontface = "bold", hjust = 0.1)
  }
  else if(!all_slots){
    credits_plot <-
      credits_plot +
      ggplot2::facet_wrap(~folding_slot, ncol=1, ) +
      ggplot2::geom_label(data = credits_per_slot,
                          inherit.aes = FALSE,
                          ggplot2::aes(x = x, y = y - (0.05 * y),
                                       label = paste(
                                         "Total Credits: ",
                                         scales::comma(total_credits_attributed)
                                       ),
                                       fill = folding_slot),
                          colour = "white",
                          fontface = "bold", hjust = 0) +
      ggplot2::geom_label(data = credits_per_slot,
                          inherit.aes = FALSE,
                          ggplot2::aes(x = x, y = y - (0.2 * y),
                                       label = paste(
                                         "Credits Per Day: ",
                                         scales::comma(mean_credits_attributed)
                                       ),
                                       fill = folding_slot),
                          colour = "white",
                          fontface = "bold", hjust = 0) +
      ggplot2::geom_label(data = credits_per_slot,
                          inherit.aes = FALSE,
                          ggplot2::aes(x = x, y = y - (0.35 * y),
                                       label = paste(
                                         "Credits Per Work Unit: ",
                                         scales::comma(credits_per_wu)
                                       ),
                                       fill = folding_slot),
                          colour = "white",
                          fontface = "bold", hjust = 0) +
      ggplot2::theme(legend.position = "none")
  }

  credits_plot
}

#' FAH Website Palette
#' @export
fah_web_palette <- c("#C74707", "#383838", "#FE6215", "#D06530", "#E7E7E7")

# FAH Categorical Palette
fah_categorical_palette <- c("#FF0802", "#04F0FF", "#FDEA05", "#FF02EF", "#0915E8")

#' Plot Cumulative Network Usage
#' @export
plot_cumulative_network_usage <- function(network_usage_daily_summary) {

  cumulative_usage_by_slot <-
    network_usage_daily_summary %>%
    dplyr::group_by(folding_slot, network_direction, log_date) %>%
    dplyr::summarise(total_usage_mib = sum(total_usage_mib)) %>%
    dplyr::arrange(folding_slot, network_direction, log_date) %>%
    dplyr::group_by(folding_slot, network_direction) %>%
    dplyr::mutate(cumulative_usage_mib = cumsum(total_usage_mib))

  cumulative_usage <-
    network_usage_daily_summary %>%
    dplyr::arrange(log_date) %>%
    dplyr::group_by(log_date) %>%
    dplyr::summarise(total_usage_mib = sum(total_usage_mib)) %>%
    dplyr::mutate(cumulative_usage_mib = cumsum(total_usage_mib))

  total_usage_gib = sum(cumulative_usage$total_usage_mib) / 1024

  cumulative_plot <-
    cumulative_usage %>%
    ggplot2::ggplot(ggplot2::aes(log_date, cumulative_usage_mib / 1024,
                                 label = round(cumulative_usage_mib / 1024, 2))) +
    ggplot2::geom_line(colour = fah_web_palette[2]) +
    ggplot2::geom_point(colour = fah_web_palette[2]) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Total Cumulative Network Usage",
                  subtitle = "Upload + Download",
                  x = "Date", y = "Cumulative Usage (GiB)") +
    ggplot2::ylim(c(0, total_usage_gib))

  cumulative_plot_by_slot <-
    cumulative_usage_by_slot %>%
    ggplot2::ggplot(ggplot2::aes(log_date, cumulative_usage_mib / 1024, fill = network_direction)) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top",
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = fah_web_palette) +
    ggplot2::facet_wrap(~folding_slot, ncol = 1) +
    ggplot2::labs(title = "Cumulative Network Usage by Folding Slot",
         x = "Date", y = "Cumulative Usage (GiB)") +
    ggplot2::ylim(c(0, total_usage_gib))

  gridExtra::grid.arrange(cumulative_plot_by_slot,
                          cumulative_plot,
                          heights = c(2, 1))
}
