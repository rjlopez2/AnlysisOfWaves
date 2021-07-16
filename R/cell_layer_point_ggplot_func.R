#' cell_layer_point_ggplot_func
#'
#' Create a dot plot ggplot object layer on a base canvas plot. This function make dots plot with aggregated waves (average) by cells, that is, every datapoint represent the average value of multiples waves of single cells. Note: this function is wrapped inside the final superplot function.
#'
#' @param ggplot_obj A ggplot object. the base canvas plot which you want to add the layer to.
#' @param dataset A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param yaxe A string. The variable name of interest to plot.
#' @param jitter_width A double. Value assigned to the jitter width. Default to `3.5`.
#' @param cell_size A double. Value assigned to the size of the dots. Default to `2`.
#' @param cell_alpha A double. Value assigned to the transparency of the dots. Default to `0.4`.
#'
#' @return  A ggplot object with a dotplot additional layer of individual cell datapoint.
#' @export
#'
#' @examples # the example is missing
cell_layer_point_ggplot_func <- function(ggplot_obj,
                                         dataset,
                                         yaxe,
                                         jitter_width = 3.5,
                                         cell_size = 2,
                                         cell_alpha = 0.4){

  yaxe <- sym(yaxe)

  WT_fct <- dataset$Animal %>%
    levels() %>%
    dplyr::nth(1)
  CPVT_fct <- dataset$Animal %>%
    levels() %>%
    dplyr::nth(2)

  cell_level_data <- dataset %>%
    group_by(.data$Animal_No,
             .data$Animal,
             .data$Condition,
             .data$Experiment) %>%
    summarise(across(where(is.double), # aggregate (averaging) by cells
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop_last")

  ggplot_obj <- ggplot_obj +
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {cell_level_data %>%
        filter(.data$Animal == WT_fct)},
        ggplot2::aes(y = {{yaxe}},
                     fill = interaction(.data$Animal, .data$Animal_No, .data$Experiment, sep = "_")),
        priority = "density",
        cex= jitter_width,
        size = cell_size,
        shape = 21,
        alpha = cell_alpha) +

    colorspace::scale_fill_discrete_sequential(palette = "Blues",
                                               l2 = 50,
                                               c2 = 10,
                                               h2 = 200) +
    # ## CPVT layer
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {cell_level_data %>%
        filter(.data$Animal == CPVT_fct)},
        ggplot2::aes(y = {{yaxe}},
                     fill = interaction(.data$Animal, .data$Animal_No, .data$Experiment, sep = "_")),
        priority = "density",
        cex= jitter_width,
        size = cell_size,
        shape = 21,
        alpha = cell_alpha) +
    colorspace::scale_fill_discrete_sequential(palette ="Reds",
                                               l1 = 10, l2 = 80,
                                               c1 = 150, c2 = 200)
  rm(cell_level_data)

  return(ggplot_obj)

}
