#' my_cum_occu_wave_plot_func
#'
#' plot cum occurence of waves
#'
#' @param my_dataset A dataframe with raw data of waves.
#' @param line_size Integer. the size of the lines to display. Default to 1.
#' @param reffer_wave_thres A double. Reference value in seconds indicating the threshold for the calculus of cumulative waves occurrence.
#' @param ... Additional paramters to pass to the eastethic setting function `pptx_presentation_theme_func()`.
#'
#' @return A ggplot object (list) ploting cumulative occurrence of waves in the different animals groups.
#' @export
#'
#' @examples # The example is still missing...
my_cum_occu_wave_plot_func <- function(my_dataset,
                                       reffer_wave_thres,
                                       line_size = 1,
                                       ...){

  my_cum_occu_plot <- my_dataset %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Wave_latency)) +
    # ggplot2::geom_rect(ggplot2::aes(ymin = -Inf,
    #                                 ymax = Inf,
    #                                 xmin = 0,
    #                                 xmax = reffer_wave_thres),
    #                    alpha = 0.5, fill = "#f0f0f0",) +
    ######## adding vertical refference line alone
    ggplot2::geom_vline(xintercept = reffer_wave_thres,
                        color = "blue",
                        size = line_size * 0.3) +
    # facet_grid(~Condition) +
    # geom_histogram(aes(fill = Condition))
    # stat_ecdf(aes(color = interaction(Animal, Condition)), na.rm = T, show.legend = F) #+
    ggplot2::stat_ecdf(ggplot2::aes(color = .data$Animal,
                                    group = interaction(.data$Animal, .data$Condition),
                                    linetype = .data$Condition),
                       na.rm = T,
                       show.legend = T,
                       pad = T,
                       size = line_size) +
    pptx_presentation_theme_func(...) +
    ggplot2:: scale_colour_manual(values = c("#666666", "#CC0000")) +
    ggplot2::scale_y_continuous(labels = function(x){x*100}) +
    ggplot2::labs(x = "Time (s)", y = "Cumulative wave\noccurrence (%)") +
    ggplot2::theme(legend.position = c(0.9, 0.3))
  # theme(legend.key=element_blank())

  return(my_cum_occu_plot)

}
