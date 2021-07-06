#' wave_layer_point_ggplot_func
#'
#' Create a dot plot ggplot object layer on a base canvas plot. This function make single waves dot plot with the raw data that is, no aggregation on single waves for multiples cells. Note: this function is wrapped inside the final superplot function.
#'
#' @param base_ggplot_obj A ggplot object. the base canvas plot which you want to add the layer to.
#' @param dataset A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param xaxe A string. The variable you want to compare with. Default to `Animal`.
#' @param yaxe A string. The variable name of interest to plot.
#' @param jitter_width A double. Value assigned to the jitter width. Default to `3.5`.
#' @param wave_size A double. Value assigned to the size of the dots. Default to `2`.
#' @param wave_alpha A double. Value assigned to the transparency of the dots. Default to `0.4`.
#'
#' @return A ggplot object with an dotplot additional layer.
#' @export
#'
#' @examples
wave_layer_point_ggplot_func <- function(base_ggplot_obj,
                                         dataset,
                                         xaxe = "Animal",
                                         yaxe,
                                         jitter_width = 3.5,
                                         wave_size = 2,
                                         wave_alpha = 0.4){

  xaxe <- sym(xaxe)
  yaxe <- sym(yaxe)

  WT_fct <- dataset$Animal %>%
    levels() %>%
    dplyr::nth(1)
  CPVT_fct <- dataset$Animal %>% #
    levels() %>%
    dplyr::nth(2)
  base_ggplot_obj <- base_ggplot_obj +
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {dataset %>%
        filter(.data$Animal == WT_fct)},
        ggplot2::aes(y = {{yaxe}},
                     fill = interaction({{xaxe}}, .data$Animal_No, sep = "_")),
        priority = "density",
        cex= jitter_width,
        size = wave_size,
        shape = 21,
        alpha = wave_alpha) +
    colorspace::scale_fill_discrete_sequential(palette = "Blues",
                                               l2 = 50,
                                               c2 = 10,
                                               h2 = 200) +
    ## CPVT layer
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {dataset %>%
        filter(.data$Animal == CPVT_fct)},
        ggplot2::aes(y = {{yaxe}},
                     fill = interaction({{xaxe}},
                                        .data$Animal_No, sep = "_")),
        priority = "density",
        cex= jitter_width,
        size = wave_size,
        shape = 21,
        alpha = wave_alpha) +
    colorspace::scale_fill_discrete_sequential(palette ="Reds",
                                               l1 = 10, l2 = 80,
                                               c1 = 150, c2 = 200)

  return(base_ggplot_obj)
}
