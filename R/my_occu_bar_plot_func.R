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
    mutate(new_waves = if_else(df40_o$Wave_latency <= reffer_wave_thres &
                                 !is.na(df40_o$Wave_latency) &
                                 df40_o$Wave_latency > 0, T, F)) %>%
    group_by(Animal, Condition, new_waves) %>%
    select(Animal, Condition, new_waves) %>%
    summarise(count = n()) %>%
    # View
    mutate(Percentage = count/sum(count) * 100) %>%
    filter(new_waves == T) %>%
    # View
    ggplot(aes(x = Animal,
               y = Percentage,
               fill = new_waves,
               alpha = 0.5))  +
    geom_bar(stat="identity",
             color = "black",
             aes(fill = Animal),
             size = 1) +
    scale_y_continuous(limits = c(0, 100)) +
    facet_grid(. ~ Condition) +
    pptx_presentation_theme_func() +
    scale_fill_manual(values = c("#666666", "#CC0000")) +
    labs(x = "Animal", y = "Wave occurrence (%)") + # Add legend
    theme(legend.position = "none")

  # my_bar_plot

  return(list(occu_bar_plot = my_bar_plot))


}
