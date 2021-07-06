#' cell_layer_vio_ggplot_func
#'
#' Create a base canvas violin ggplot  object. This function make violin plot with aggregated waves (average) at the cellular level. Note: this function is wrapped inside the final superplot function.
#'
#' @param dataset A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param yaxe A string. The variable name of interest to plot.
#' @param xaxe A string. The variable you want to compare with. Default to `Animal`.
#' @param faceted_by_1 A string. Grouping variables for visualization. Default to `Condition`.
#' @param faceted_by_2 A string. Aditional grouping variables for visualization. Default to `.`.
#'
#' @return A ggplot object with a violin plot canvas to add additional layers.
#' @export
#'
#' @examples
cell_layer_vio_ggplot_func <- function(dataset,
                                       yaxe,
                                       xaxe = "Animal",
                                       faceted_by_1 = "Condition",
                                       faceted_by_2 = "."){


  cell_level_data <- dataset %>%
    group_by(.data$Animal_No,
             .data$Animal,
             .data$Condition,
             .data$Experiment) %>%
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
                         size = 1) +
    ggplot2::facet_grid(stats::reformulate(faceted_by_1, faceted_by_2)) +
    ggplot2::scale_colour_manual(values = c("#666666", "#CC0000"))


  return(base_plot)

}
