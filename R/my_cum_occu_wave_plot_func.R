#' my_cum_occu_wave_plot_func
#'
#' plot cum occurence of waves
#'
#' @param my_dataset A dataframe with raw data of waves.
#' @param reffer_wave_thres A double. Reference value i n seconds indicating the threshold for the calculus of cumulative waves occurrence.
#' @param font_size
#'
#' @return
#' @export
#'
#' @examples # The example is still missing...
my_cum_occu_wave_plot_func <- function(my_dataset, reffer_wave_thres){

  my_cum_occu_plot <- my_dataset %>%
    ggplot(aes(x = Wave_latency)) +
    geom_rect(aes(ymin = -Inf,
                  ymax = Inf,
                  xmin = 0,
                  xmax = reffer_wave_thres),
              alpha = 0.5, fill = "#f0f0f0",) +
    # facet_grid(~Condition) +
    # geom_histogram(aes(fill = Condition))
    # stat_ecdf(aes(color = interaction(Animal, Condition)), na.rm = T, show.legend = F) #+
    stat_ecdf(aes(color = Animal,
                  group = interaction(Animal, Condition),
                  linetype = Condition),
              na.rm = T,
              show.legend = T,
              pad = T,
              lwd = 1) +
    # pptx_presentation_theme_func(font_size) +
    scale_colour_manual(values = c("#666666", "#CC0000")) +
    scale_y_continuous(labels = function(x){x*100}) +
    labs(x = "Time (s)", y = "Cumulative wave\noccurrence (%)") +
    theme(legend.position = c(1, 0.4),
          legend.key=element_blank())
  # theme(legend.key=element_blank())

  return(list(cum_occu_plot = my_cum_occu_plot))

}
