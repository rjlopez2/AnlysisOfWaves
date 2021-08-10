#' Plotting theme for figures
#'
#' Modify default ggplot2 object aesthetics for the figures of the paper.
#'
#' @param base_size An integer. Specify the font and other element's size.
#'
#' @return A plot with modified aesthetics theme.
#' @export
#' @details This take a ggplot2 object, usually a scatter plot and change the theme to almost totally clear.
#' @examples # The example is still missing...
#'
pptx_presentation_theme_func <- function(base_size = 26){
  structure(list(
    axis.title = ggplot2::element_text(size = base_size * 1.2),
                              # vjust = 1),
    axis.text = ggplot2::element_text(size = base_size ,
                             # vjust = 1,
                             colour = "black"),
    # panel.border = ggplot2::element_blank(),
    # panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    # strip.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "transparent", color = NA),
    strip.text.x = ggplot2::element_text(size = base_size,
                                       face = "bold",
                                       vjust = 1),
    axis.line = ggplot2::element_line(colour = "black",
                             size = base_size / 15,
                             lineend = "round"),
    axis.ticks = ggplot2::element_line(size = base_size / 15,
                              lineend = "round",
                              colour = "black"),
    panel.border = ggplot2::element_rect(fill = "transparent", color = NA),
    panel.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the panel
    plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = ggplot2::element_rect(fill = "transparent", color = NA), # get rid of legend bg
    legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
    # axis.title.x=ggplot2::element_blank()



  ), class = c("theme", "gg"))

}
