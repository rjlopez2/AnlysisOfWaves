#' Superplot with violin and scatter plot
#'
#' Make a violin and scatter plot with aggregated data at different levels (animal, cell, etc).
#'
#'@param dataset A dataframe with raw data of waves.
#' @param xaxe String. name of the variable in the x axis.
#' @param yaxe String. name of the variable in the y axis.
#' @param faceted_by_1 String. Define facet for plotting.
#' @param faceted_by_2 String. Define facet for plotting.
#' @param grouping String vector of lenght >=1. Names of the grouping variables to compute summarize stats.
#' @param jitter_width Double. Define how big is the with of the scatter points.
#' @param cell_size Double. define the size of the cell level scatter plot.
#' @param cell_alpha Double. define the transparency of the cell level scatter plot.
#' @param animal_size Double. define the size of the animal level scatter plot.
#' @param animal_alpha Double. define the transparency of the animal level scatter plot.
#' @param ... Other parameters to pass to the function.
#'
#' @return A ggplot object with boxplot + jitter scatter plot.
#' @export
#'
#' @examples # The example is still missing...

superplot_func_test <- function(dataset, xaxe, yaxe,
                                faceted_by_1 = "Condition",
                                faceted_by_2 = ".",
                                grouping = c("Animal",
                                             "Treatment",
                                             "Animal_No",
                                             "Condition"),
                                jitter_width = 3.5,
                                cell_size = 2,
                                cell_alpha = 0.4,
                                animal_size = 4,
                                animal_alpha = 0.8,
                                ...){
  WT_fct <- dataset$Animal %>%
    levels() %>%
    dplyr::nth(1)
  CPVT_fct <- dataset$Animal %>%
    levels() %>%
    dplyr::nth(2)

  xaxe <- sym(xaxe)
  yaxe <- sym(yaxe)
  yaxe_mean <- sym(paste("mean", yaxe, sep = "_"))

  ## Create the summarized dataset (aggregated by the mean)
  summ_dataset <- dataset %>%
    group_by(across(any_of(grouping))) %>%
    # group_by(.data$Animal, .data$Animal_No, .data$Condition, any_of(.data$Treatment) ) %>%
    summarise(across(where(is.numeric), list(mean = ~ mean(.x, na.rm = TRUE)),
                     .names = "{.fn}_{.col}"), .groups = 'drop')

  ## Create base layer plot
  base_plot <-
    ggplot2::ggplot(data = dataset,
                    ggplot2::aes(x = {{xaxe}},
                                 y = {{yaxe}})) +
    ggplot2::geom_violin(ggplot2::aes(color = {{xaxe}})) +
    ggplot2::facet_grid(stats::reformulate(faceted_by_1, faceted_by_2)) +
    ggplot2::scale_colour_manual(values = c("#666666", "#CC0000"))


  base_plot <- base_plot +
    ## Add Cell level data (SMALL scatterplot)
    ## WT layer
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {dataset %>%
        filter(.data$Animal == WT_fct)},
        ggplot2::aes(y = {{yaxe}},
                     fill = interaction({{xaxe}}, .data$Animal_No, sep = "_")),
        priority = "density",
        cex= jitter_width,
        size = cell_size,
        shape = 21,
        alpha = cell_alpha) +
    colorspace::scale_fill_discrete_sequential(palette = "Blues",
                                               l2 = 50,
                                               c2 = 10,
                                               h2 = 200) +
    ## CPVT layer
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {dataset %>%
        filter(.data$Animal == CPVT_fct)},
        ggplot2::aes(y = {{yaxe}},
                     fill = interaction({{xaxe}}, .data$Animal_No, sep = "_")),
        priority = "density",
        cex= jitter_width,
        size = cell_size,
        shape = 21,
        alpha = cell_alpha) +
    colorspace::scale_fill_discrete_sequential(palette ="Reds",
                                               l1 = 10, l2 = 80,
                                               c1 = 150, c2 = 200) +
    ## Add Animal level data (BIG scatterplot)
    ## WT layer
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {summ_dataset %>%
        filter(.data$Animal == WT_fct)},
        ggplot2::aes(y = {{yaxe_mean}},
                     fill = interaction(.data$Animal, .data$Animal_No, sep = "_")),
        size = animal_size,
        shape = 21,
        alpha = animal_alpha) +
    colorspace::scale_fill_discrete_sequential(palette = "Blues",
                                               l2 = 50,
                                               c2 = 10,
                                               h2 = 200) +
    ## CPVT layer
    ggnewscale::new_scale("fill") +
    ggbeeswarm::geom_beeswarm(data = {summ_dataset %>%
        filter(.data$Animal == CPVT_fct)},
        ggplot2::aes(y = {{yaxe_mean}},
                     fill = interaction(.data$Animal, .data$Animal_No, sep = "_")),
        size = animal_size,
        shape = 21,
        alpha = animal_alpha) +
    colorspace::scale_fill_discrete_sequential(palette ="Reds",
                                               l1 = 10, l2 = 80,
                                               c1 = 150, c2 = 200) +
    # remove legend
    ggplot2::theme(legend.position="none") +
    pptx_presentation_theme_func()


  rm(summ_dataset)
  return(base_plot)

}
