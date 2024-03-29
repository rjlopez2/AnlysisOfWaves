#' cell_layer_vio_ggplot_func
#'
#' Create a base canvas violin ggplot  object. This function make violin plot with aggregated waves (average) at the cellular level. Note: this function is wrapped inside the final superplot function.
#'
#' @param dataset A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param yaxe A string. The variable name of interest to plot.
#' @param xaxe A string. The variable you want to compare with. Default to `Animal`.
#' @param faceted_by_1 A string. Grouping variables for visualization. Default to `Condition`.
#' @param faceted_by_2 A string. Additional grouping variables for visualization. Default to `.`.
#' @param my_grouping_vars Character vector. A character vector of groups names assigned to perform the cell aggregation. Don't change at least you know what you are doing!
#' @param y_limits A pair or single double value. Optional parameter to set the upper and lower y axis limit of your plot. If a single value provided, it will be used as upper limit.
#' @param line_size Integer. the size of the lines to display.
#' @param trim Logic. shall the edges of the violin plots be cutted off?. Default to FALSE
#' @param scaled_to Character string. If "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
#' @param adjusted Double. A multiplicate bandwidth adjustment. This makes it possible to adjust the bandwidth while still using the a bandwidth estimator. For example, adjust = 1/2 means use half of the default bandwidth.
#' @param alpha_line Double. Adjust transparency to the violin line.
#'
#' @importFrom methods missingArg
#' @return A ggplot object with a violin plot canvas to add additional layers.
#' @export
#'
#' @examples # the example is missing
cell_layer_vio_ggplot_func <- function(dataset,
                                       yaxe,
                                       xaxe = "Animal",
                                       my_grouping_vars = c("Animal_No", "Animal", "Condition", "Treatment", "Experiment"),
                                       faceted_by_1 = "Condition",
                                       faceted_by_2 = ".",
                                       y_limits,
                                       alpha_line = 1,
                                       line_size = 1,
                                       trim = FALSE,
                                       scaled_to = "width",
                                       adjusted = 1){

  cell_level_data <- dataset %>%
    # group_by(.data$Animal_No,
    #          .data$Animal,
    #          .data$Condition,
    #          .data$Experiment) %>%
    group_by(across(any_of(my_grouping_vars))) %>%
    summarise(across(where(is.double), # aggregate (averaging) by cells
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop_last")



  xaxe <- sym(xaxe)
  yaxe <- sym(yaxe)
  # yaxe_mean <- sym(paste("mean", yaxe, sep = "_")) # no needed here

  base_plot <-
    ggplot2::ggplot(data = cell_level_data,
                    ggplot2::aes(x = {{xaxe}},
                                 y = {{yaxe}})) +
    ggplot2::geom_violin(ggplot2::aes(color = {{xaxe}}),
                         size = line_size,
                         alpha = alpha_line,
                         # draw_quantiles = 0.5,
                         trim = trim,
                         scale = scaled_to,
                         adjust = adjusted) +
    ggplot2::facet_grid(stats::reformulate(faceted_by_1, faceted_by_2)) +
    ggplot2::scale_colour_manual(values = c("#666666", "#CC0000"))


  if(!missingArg(y_limits)){

    attempt::stop_if(!is.double(y_limits), msg = "Invalid vlaue. You must provide a value of type double to the argument y_limits.")

    if(length(y_limits) == 1){

      base_plot <- base_plot +
        ggplot2::coord_cartesian(ylim = c(0, y_limits))
    }

    if(length(y_limits) == 2){

      base_plot <- base_plot +
        ggplot2::coord_cartesian(ylim = y_limits)
    }

    if(length(y_limits) > 2){
      stop("You must provide a vector of maximun length == 2  to the argument y_limits.")
    }

  }


  # if(!missingArg(y_limits)){
  #
  #   base_plot <- base_plot +
  #     ggplot2::coord_cartesian(ylim = c(0, y_limits))
  # }


  return(base_plot)

}
