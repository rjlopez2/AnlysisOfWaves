#' Boxplot with jitter scatter points
#'
#' This function makes a boxplot and jitter plot for animal comparisons. It needs at least two variable x, y to be plotted.
#'
#' @param dataset A dataframe with raw data of waves.
#' @param xaxe String. name of the variable in the x axis.
#' @param yaxe String. name of the variable in the y axis.
#' @param box_color String. Defining the color for whisker box.
#' @param scatt_color String. Defining the color for scatter points.
#' @param faceted_by_1 String. Define facet for plotting.
#' @param faceted_by_2 String. Define facet for plotting.
#' @param jitter_width Double. Define how big is the with of the scatter points.
#' @param .alpha Double. Define transparency for your scatter points.
#' @param .dot_size Double. Define the size of the scatter points.
#' @param base_font_size A integer. Modify the size of the fonts. Default to 22.
#'
#' @return A ggplot object with boxplot + jitter scatter plot.
#' @export
#'
#' @examples # The example is still missing...
my_boxplot_and_jitter_func <- function(dataset,
                                       xaxe = "Animal",
                                       yaxe,
                                       box_color  = "Animal",
                                       scatt_color = "Animal",
                                       faceted_by_1 = "Condition",
                                       faceted_by_2 = ".",
                                       jitter_width = 0.5,
                                       .alpha = 0.25,
                                       .dot_size = 2.5,
                                       base_font_size = 22) {

  scatt_color <- paste0("interaction(", paste0(scatt_color, collapse =  ", "), ")")
  box_color <- paste0("interaction(", paste0(box_color, collapse =  ", "), ")")

  ggplot2::ggplot(data = dataset,
                  ggplot2::aes_string(x = xaxe,
                    y = yaxe)) +
    ggplot2::geom_boxplot(ggplot2::aes_string(color = box_color),
                 outlier.shape = NA,
                 lwd = 1,
                 show.legend = F) +
    # aes(x = {{xaxe}},
    #     y = {{yaxe}})) +
    # geom_boxplot(aes(color = {{box_color}}),
    #              outlier.shape = NA, # Remove outlier
    #              show.legend = F) +
    ggplot2::geom_point(shape = 21,
               size = .dot_size,
               ggplot2::aes_string(fill = box_color,
                          group = scatt_color),
               color = "black",
               alpha = .alpha,
               position = ggplot2::position_jitterdodge(jitter.width = jitter_width,  # add jitter
                                               seed = 999),
               show.legend = F) +
    ggpubr::stat_compare_means(paired = F, # this compute p.values
                       show.legend = F,
                       # label = "p.signif", # this shows the "*" symbols significance code
                       label.x.npc = "centre",
                       size = 5) +
    # symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
    #                    symbols = c("****", "***", "**", "*", "ns"))) +
    # stat_summary(fun = mean, # this shows the mean (string) on the graph
    #              geom = "text",
    #              color = "black",
    #              vjust = -0.7,
    #              show.legend = F,
    #              aes( label = round(..y.., digits = 2))) +

    ggplot2::facet_grid(stats::reformulate(faceted_by_1, faceted_by_2)) + # facet by ...
    # labs(subtitle = get_test_label(, detailed = TRUE)) + # shows detailed legend on statistics
    ggplot2::scale_colour_manual(values = c("#666666", "#CC0000")) + # set to red and black as defoult color for Animals
    ggplot2::scale_fill_manual(values = c("#666666", "#CC0000")) + # set to red and black as defoult color for Animals
  # scale_x_discrete(labels = c("WT", "CPVT")) # rename x-axis
    pptx_presentation_theme_func(base_font_size)

}
