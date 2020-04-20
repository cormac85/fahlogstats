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
  credits_plot <- credits_df %>%
    ggplot() +
    geom_col(aes(log_date, credits_attributed, fill = as.character(log_time)),
             position = "stack") +
    theme_minimal() +
    scale_x_date(date_breaks = "1 day", date_labels = "%a %F") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(title = paste0(unique(credits_df$folding_slot, collapse = " - "),
                        ": Credits Acquired Per Day"),
         subtitle = paste0(min(credits_df$log_date), " - ",
                           max(credits_df$log_date)),
         x = "Date", y = "Credits") +
    scale_y_continuous(labels = scales::comma_format())

  if(all_slots)
    credits_plot <- credits_plot
  else if(!all_slots)
    credits_plot <- credits_plot + facet_wrap(~folding_slot, ncol=1)

  credits_plot
}



# cumulative_plot_by_slot <-
#   network_usage_daily_summary %>%
#   arrange(folding_slot, network_direction, log_date) %>%
#   group_by(folding_slot, network_direction) %>%
#   mutate(cumulative_usage_mib = cumsum(total_usage_mib)) %>%
#   ggplot(aes(log_date, cumulative_usage_mib / 1024, fill = network_direction)) +
#   geom_col() +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   scale_fill_brewer(palette = "Set2") +
#   facet_wrap(~folding_slot, ncol = 1) +
#   labs(title = "Cumulative Network Usage by Folding Slot",
#        x = "Date", y = "Cumulative Usage (GiB)") +
#   ylim(c(0, total_usage_gib))
#
# cumulative_plot <-
#   network_usage_daily_summary %>%
#   arrange(folding_slot, network_direction, log_date) %>%
#   group_by(folding_slot, network_direction) %>%
#   mutate(cumulative_usage_mib = cumsum(total_usage_mib))  %>%
#   group_by(log_date) %>%
#   mutate(cumulative_usage_mib = sum(cumulative_usage_mib)) %>%
#   ggplot(aes(log_date, cumulative_usage_mib / 1024)) +
#   geom_line(colour = RColorBrewer::brewer.pal(3, "Set2")[3], size = 1) +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(title = "Total Cumulative Network Usage",
#        subtitle = "Upload + Download",
#        x = "Date", y = "Cumulative Usage (GiB)") +
#   ylim(c(0, total_usage_gib))
#
#
# gridExtra::grid.arrange(cumulative_plot_by_slot,
#                         cumulative_plot,
#                         heights = c(2, 1))
