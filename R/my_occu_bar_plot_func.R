#' my_occu_bar_plot_func
#'
#' bar plot of waves occurrence
#'
#' @param my_dataset A dataframe with raw data of waves.
#' @param reffer_wave_thres A double. Reference value in seconds indicating the threshold for the calculus of cumulative waves occurrence.
#'
#' @return A ggplot object with barplot of waves occurrence in the different animals groups.
#' @export
#'
#' @examples # The example is still missing...
my_occu_bar_plot_func <- function(my_dataset, reffer_wave_thres){

  my_bar_plot <- my_dataset %>%
    mutate(new_waves = if_else(.data$Wave_latency <= reffer_wave_thres &
                                 !is.na(.data$Wave_latency) &
                                 .data$Wave_latency > 0, T, F)) %>%
    group_by(.data$Animal, .data$Condition, .data$new_waves) %>%
    select(.data$Animal, .data$Condition, .data$new_waves) %>%
    summarise(count = n()) %>%
    # View
    mutate(Percentage = count/sum(count) * 100) %>%
    filter(.data$new_waves == T) %>%
    # View
    ggplot2::ggplot(ggplot2::aes(x = .data$Animal,
                                 y = .data$Percentage,
                                 fill = .data$new_waves,
                                 alpha = 0.5))  +
    ggplot2::geom_bar(stat="identity",
                      color = "black",
                      ggplot2::aes(fill = .data$Animal),
                      size = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::facet_grid(. ~ .data$Condition) +
    pptx_presentation_theme_func() +
    ggplot2::scale_fill_manual(values = c("#666666", "#CC0000")) +
    ggplot2::labs(x = "Animal", y = "Wave occurrence (%)") + # Add legend
    ggplot2::theme(legend.position = "none")

  # my_bar_plot

  return(list(occu_bar_plot = my_bar_plot))


}
