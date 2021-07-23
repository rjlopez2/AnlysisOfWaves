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
                                       y_limits){

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
                         size = 1,
                         fill = NA) +
    ggplot2::facet_grid(stats::reformulate(faceted_by_1, faceted_by_2)) +
    ggplot2::scale_colour_manual(values = c("#666666", "#CC0000"))

  if(!missingArg(y_limits)){

    base_plot <- base_plot +
      ggplot2::coord_cartesian(ylim = c(0, y_limits))
  }


  return(base_plot)

}
