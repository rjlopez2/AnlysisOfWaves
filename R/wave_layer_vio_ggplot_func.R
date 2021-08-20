#' wave_layer_vio_ggplot_func
#'
#' Create a base canvas violin ggplot  object. This function make single waves violin plot with the raw data that is, no aggregation on single waves for multiples cells. Note: this function is wrapped inside the final superplot function.
#'
#' @param dataset A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param yaxe A string. The variable name of interest to plot.
#' @param xaxe A string. The variable you want to compare with. Default to `Animal`.
#' @param faceted_by_1 A string. Grouping variables for visualization. Default to `Condition`.
#' @param faceted_by_2 A string. Aditional grouping variables for visualization. Default to `.`.
#' @param y_limits A double. Optional paramter to set the upper y limit to your plot.
#' @param line_size Integer. the size of the lines to display. Default to 1.
#' @param trim Logic. shall the edges of the violin plots be cutted off?. Default to FALSE
#' @param scaled_to Character string. If "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
#' @param adjusted Double. A multiplicate bandwidth adjustment. This makes it possible to adjust the bandwidth while still using the a bandwidth estimator. For example, adjust = 1/2 means use half of the default bandwidth.
#'
#' @importFrom methods missingArg
#' @return A ggplot object with a violin plot canvas to add additional layers.
#' @export
#'
#' @examples # the example is missing
wave_layer_vio_ggplot_func <- function(dataset,
                                       yaxe,
                                       xaxe = "Animal",
                                       faceted_by_1 = "Condition",
                                       faceted_by_2 = ".",
                                       y_limits,
                                       line_size = 1,
                                       trim = FALSE,
                                       scaled_to = "width",
                                       adjusted = 1){

  ####################################################################################
  ### this function make single waves violin plot with the raw data
  ### that is, no aggregation on single waves for multiples cells
  ####################################################################################

  xaxe <- sym(xaxe)
  yaxe <- sym(yaxe)
  # yaxe_mean <- sym(paste("mean", yaxe, sep = "_")) # no needed here

  base_plot <-
    ggplot2::ggplot(data = dataset,
                    ggplot2::aes(x = {{xaxe}},
                                 y = {{yaxe}})) +
    ggplot2::geom_violin(ggplot2::aes(color = {{xaxe}}),
                         size = line_size,
                         fill = NA,
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


  return(base_plot)

}
