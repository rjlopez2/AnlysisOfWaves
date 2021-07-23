#' Superplot with violin and scatter plot
#'
#' Make a violin and scatter plot with aggregated data at different levels (animal, cell or waves). The layers are constructed in this order: (wave/cell) violin layer -> (wave/cell) small dot layer -> (Animal) large dot layer. Note: this function is depending on other functions to build the multiples layers up.
#'
#' @param dataset A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param yaxe A string. The variable name of interest to plot.
#' @param base_violin A string. Which base plot would you like to plot as a violin plot?. Options are: 1. `waves`, 2. `cells`. Default to `cells`.
#' @param dot_layer A string. Which data would you like to see in the next layer?. Options are: 1. `waves`, 2. `cells`, 3. `waves_and_cells`, 4. `none`. Default to `cells`.
#' @param animal_layer A logic value. Would you like to add the Animal dot layer? default to `TRUE`.
#' @param wave_size A double. Value assigned to the size of the dots. Default to `2`.
#' @param wave_alpha A double. Value assigned to the transparency of the dots. Default to `0.4`.
#' @param cell_size A double. Value assigned to the size of the dots. Default to `2.5`.
#' @param cell_alpha A double. Value assigned to the transparency of the dots. Default to `0.4`.
#' @param animal_size A double. Value assigned to the size of the dots. Default to `5`.
#' @param animal_alpha A double. Value assigned to the transparency of the dots. Default to `0.6`.
#' @param base_font_size A integer. Modify the size of the fonts. Default to 15.
#' @param ... Pass additional parameters to the internla plot layers.
#'
#' @return A ggplot object with a superplot indicating multiples layers of  Animals, cell or waves.
#' @export
#'
#' @examples # the example is missing
superplot_func <- function(dataset,
                           yaxe,
                           base_violin = "cells",
                           dot_layer = "cells",
                           animal_layer = TRUE,
                           wave_size = 2,
                           wave_alpha = 0.4,
                           cell_size = 2.5,
                           cell_alpha = 0.4,
                           animal_size = 5,
                           animal_alpha = 0.6,
                           base_font_size = 15,
                           ...){

  ####################################################################################
  ### this function the different plotting layers for the final superplot
  ### may be need some twitch for better flexibility
  ####################################################################################

  # create base violin plot canvas based on waves or cells, default to cells
  switch (base_violin,

          waves = superplot <- dataset %>%
            wave_layer_vio_ggplot_func(yaxe = yaxe, ...),

          cells = superplot <- dataset %>%
            cell_layer_vio_ggplot_func(yaxe = yaxe, ...),

          stop("Invalid `base_violin` value. You must select a violin basic plot. Options are: 1. `waves`, 2. `cells`")

  )

  # create cell or wave dot plot layer, default to cells
  switch (dot_layer,
          cells = superplot <- superplot %>%
            cell_layer_point_ggplot_func(dataset = dataset,
                                         yaxe = yaxe,
                                         jitter_width = 3,
                                         cell_alpha = cell_alpha,
                                         cell_size = cell_size),

          waves = superplot <- superplot %>%
            wave_layer_point_ggplot_func(dataset = dataset,
                                         yaxe = yaxe,
                                         jitter_width = 3,
                                         wave_size = wave_size,
                                         wave_alpha = wave_alpha),

          waves_and_cells = superplot <- superplot %>%
            wave_layer_point_ggplot_func(dataset = dataset,
                                         yaxe = yaxe,
                                         jitter_width = 3,
                                         wave_size = wave_size,
                                         wave_alpha = wave_alpha) %>%
            cell_layer_point_ggplot_func(dataset = dataset,
                                         yaxe = yaxe,
                                         jitter_width = 3,
                                         cell_alpha = cell_alpha,
                                         cell_size = cell_size),


          none = superplot,

          stop("Invalid `dot_layer` value. You must select a dot (small) plot type. Options are: 1. `'waves'`, 2. `'cells'`, 3. `'waves_and_cells'` or 4. `'none'`")

  )


  ##### ad last layer with animal dots (big)
  if(animal_layer){
    superplot <- superplot %>%
      animal_layer_point_ggplot_func(dataset = dataset,
                                     yaxe = yaxe,
                                     animal_size = animal_size,
                                     animal_alpha = animal_alpha,
                                     jitter_width = 0) +
      ggplot2::theme(legend.position = "none") +
      pptx_presentation_theme_func(base_font_size)
  }

  superplot <- superplot +
    ggplot2::theme(legend.position = "none") +
    pptx_presentation_theme_func(base_font_size)


  return(superplot)

}
