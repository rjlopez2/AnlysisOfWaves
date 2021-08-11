#' my_cum_occu_wave_plot_func
#'
#' plot cum occurence of waves
#'
#' @param my_dataset A dataframe with raw data of waves.
#' @param my_var A  character string of length 1. The name of the variable you wabt to plot the cumulative distribution.
#' @param line_size Integer. the size of the lines to display. Default to 1.
#' @param reffer_wave_thres A double. Reference value in seconds indicating the threshold for the calculus of cumulative waves occurrence.
#' @param reference_line Logic. Should you add a reference veertical line?
#' @param ... Additional paramters to pass to the eastethic setting function `pptx_presentation_theme_func()`.
#'
#' @return A ggplot object (list) ploting cumulative occurrence of waves in the different animals groups.
#' @export
#'
#' @examples # The example is still missing...
my_cum_occu_wave_plot_func <- function(my_dataset,
                                       my_var,
                                       reffer_wave_thres,
                                       line_size = 1,
                                       reference_line = TRUE,
                                       ...){

  attempt::stop_if(missing(my_var), msg = "No argument provided for `my_var`. You must provide a target variable to make the plot.")
  attempt::stop_if(!is.character(my_var), msg = "No valid value for argument `my_var`. You must provide a value of type `character`.")
  attempt::stop_if(length(my_var) > 1, msg = "You must provide a single variable name to make the plot.")

  my_var <- sym(my_var)
  my_cum_occu_plot <- my_dataset %>%
    ggplot2::ggplot(ggplot2::aes(x = !!my_var)) +
    # ggplot2::geom_rect(ggplot2::aes(ymin = -Inf,
    #                                 ymax = Inf,
    #                                 xmin = 0,
    #                                 xmax = reffer_wave_thres),
    #                    alpha = 0.5, fill = "#f0f0f0",) +
    ######## adding vertical refference line alone

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
    # ggplot2::labs(x = "Time (s)", y = paste("Cumulative", rlang::as_string(my_var),  "(%)")) +
    ggplot2::theme(legend.position = c(0.9, 0.3))

  # theme(legend.key=element_blank())
  if(reference_line){
    my_cum_occu_plot <- my_cum_occu_plot +
      ggplot2::geom_vline(xintercept = reffer_wave_thres,
                          color = "blue",
                          size = line_size * 0.5)
  }

  return(my_cum_occu_plot)

}
