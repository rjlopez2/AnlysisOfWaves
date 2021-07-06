#' animal_layer_point_ggplot_func
#'
#' Create a dot plot ggplot object layer on a base canvas plot. This function makes a dot plot by calculating the average value for each animal based on the cellular level, so it shows the average wave for each animal. Note: this function is wrapped inside the final superplot function.
#'
#' @param ggplot_obj A ggplot object. the base canvas plot which you want to add the layer to.
#' @param dataset A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param xaxe A string. The variable you want to compare with. Default to `Animal`.
#' @param colored_by A string. The criteria to assigning color to the dots. Options possibles are by `animal_no` and by `animal_type`. Default to `animal_type`.
#' @param yaxe A string. The variable name of interest to plot.
#' @param jitter_width A double. Value assigned to the jitter width. Default to `3.5`.
#' @param animal_size A double. Value assigned to the size of the dots. Default to `2`.
#' @param animal_alpha A double. Value assigned to the transparency of the dots. Default to `0.4`.
#'
#' @return A ggplot object with a dotplot additional layer of individual Animal datapoints.
#' @export
#'
#' @examples
animal_layer_point_ggplot_func <- function(ggplot_obj,
                                           dataset,
                                           xaxe = "Animal",
                                           colored_by,
                                           yaxe,
                                           jitter_width = 3.5,
                                           animal_size = 2,
                                           animal_alpha = 0.4){


  xaxe <- sym(xaxe)
  yaxe <- sym(yaxe)

  WT_fct <- dataset$Animal %>%
    levels() %>%
    dplyr::nth(1)
  CPVT_fct <- dataset$Animal %>%
    levels() %>%
    dplyr::nth(2)

  animal_level_data <- dataset %>%
    group_by(.data$Animal_No,
             .data$Animal,
             .data$Condition,
             .data$Experiment) %>%
    summarise(across(where(is.double), # aggregate (averaging) by cells
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop_last") %>%
    group_by(.data$Animal_No,
             .data$Animal,
             .data$Condition) %>%
    summarise(across(where(is.double), # aggregate (averaging) by Animal
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop_last")

  switch (colored_by,

          animal_no = ggplot_obj <- ggplot_obj +
            ggnewscale::new_scale("fill") +
            ggbeeswarm::geom_beeswarm(data = {animal_level_data %>%
                filter(.data$Animal == WT_fct)},
                ggplot2::aes(y = {{yaxe}},
                             fill = interaction({{xaxe}}, .data$Animal_No, sep = "_")),
                priority = "density",
                cex= jitter_width,
                size = animal_size,
                shape = 21,
                alpha = animal_alpha) +
            colorspace::scale_fill_discrete_sequential(palette = "Blues",
                                                       l2 = 50,
                                                       c2 = 10,
                                                       h2 = 200) +
            ggnewscale::new_scale("fill") +
            ggbeeswarm::geom_beeswarm(data = {animal_level_data %>%
                filter(.data$Animal == CPVT_fct)},
                ggplot2::aes(y = {{yaxe}},
                             fill = interaction({{xaxe}}, .data$Animal_No, sep = "_")),
                priority = "density",
                cex= jitter_width,
                size = animal_size,
                shape = 21,
                alpha = animal_alpha) +
            colorspace::scale_fill_discrete_sequential(palette ="Reds",
                                                       l1 = 10, l2 = 80,
                                                       c1 = 150, c2 = 200),


          animal_type = ggplot_obj <- ggplot_obj +
            ggnewscale::new_scale("fill") +
            ggplot2::geom_point(data = animal_level_data,
                                shape = 21,
                                size = animal_size,
                                ggplot2::aes(fill = {{xaxe}},
                                             group = {{xaxe}}),
                                color = "black",
                                alpha = animal_alpha,
                                position = ggplot2::position_jitterdodge(jitter.width = jitter_width,  # add jitter
                                                                         seed = 999)) +
            ggplot2::scale_fill_manual(values = c("#666666", "#CC0000")),


          stop("Invalid `colored_by` value. You must select a valid criteria for assigning color to the Animal plot layer. Options are: 1. `animal_type`, 2. `animal_no`")
  )


  rm(animal_level_data, xaxe, yaxe, WT_fct, CPVT_fct)

  return(ggplot_obj)

}
