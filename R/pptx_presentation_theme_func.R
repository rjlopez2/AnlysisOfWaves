#' Plot theme for publication
#'
#' Modify default ggplot2 object aesthetics for the paper.
#'
#' @param base_size An integer. Specify the font and other element's size.
#'
#' @return A plot with modified aesthetics theme.
#' @export
#' @details This take a ggplot2 object, usually a scatter plot and change the theme to almost totally clear.
#' @examples # The example is still missing...
#'
pptx_presentation_theme_func <- function(base_size = 15){
  structure(list(
    axis.title = ggplot2::element_text(size = base_size * 1.2 ,
                              vjust = 1),
    axis.text = ggplot2::element_text(size = base_size * 0.8 ,
                             vjust = 1,
                             colour = "black"),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = base_size * 0.8 ,
                              face = "bold",
                              vjust = 1),
    axis.line = ggplot2::element_line(colour = "black",
                             size = base_size / 15,
                             lineend = "round"),
    axis.ticks = ggplot2::element_line(size = base_size / 15,
                              lineend = "round",
                              colour = "black"),
    axis.title.x=ggplot2::element_blank()



  ), class = c("theme", "gg"))

}
