#' Boxplot with jitter scatter points
#'
#' @param dataset
#' @param xaxe
#' @param yaxe
#' @param box_color
#' @param scatt_color
#' @param faceted_by_1
#' @param faceted_by_2
#' @param jitter_width
#' @param .alpha
#' @param .dot_size
#' @param ...
#'
#' @return
#' @export
#'
#' @examples # # The example is still missing...
my_boxplot_and_jitter_func <- function(dataset, # this function makes a boxplot and jitter plot for animal comparison
                                       xaxe = "Animal",
                                       yaxe,
                                       box_color  = "Animal",
                                       scatt_color = "Animal",
                                       faceted_by_1 = "Treatment + Condition",
                                       faceted_by_2 = ".",
                                       jitter_width = 0.5,
                                       .alpha = 0.25,
                                       .dot_size = 2.5,
                                       ...) {

  scatt_color <- paste0("interaction(", paste0(scatt_color, collapse =  ", "), ")")
  box_color <- paste0("interaction(", paste0(box_color, collapse =  ", "), ")")

  ggplot(data = dataset,
         aes_string(x = xaxe,
                    y = yaxe)) +
    geom_boxplot(aes_string(color = box_color),
                 outlier.shape = NA,
                 lwd = 1,
                 show.legend = F) +
    # aes(x = {{xaxe}},
    #     y = {{yaxe}})) +
    # geom_boxplot(aes(color = {{box_color}}),
    #              outlier.shape = NA, # Remove outlier
    #              show.legend = F) +
    geom_point(shape = 21,
               size = .dot_size,
               aes_string(fill = box_color,
                          group = scatt_color),
               color = "black",
               alpha = .alpha,
               position = position_jitterdodge(jitter.width = jitter_width,  # add jitter
                                               seed = 999),
               show.legend = F) +
    stat_compare_means(paired = F, # this compute p.values
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

    facet_grid(reformulate(faceted_by_1, faceted_by_2)) + # facet by ...
    # labs(subtitle = get_test_label(, detailed = TRUE)) + # shows detailed legend on statistics
    pptx_presentation_theme_func(...) +
    scale_colour_manual(values = c("#666666", "#CC0000")) + # set to red and black as defoult color for Animals
    scale_fill_manual(values = c("#666666", "#CC0000"))# + # set to red and black as defoult color for Animals
  # scale_x_discrete(labels = c("WT", "CPVT")) # rename x-axis

}
