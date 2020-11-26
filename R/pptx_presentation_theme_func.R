pptx_presentation_theme_func <- function(base_size = 15){
  structure(list(
    axis.title = element_text(size = base_size * 1.2 ,
                              vjust = 1),
    axis.text = element_text(size = base_size * 0.8 ,
                             vjust = 1,
                             colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = base_size * 0.8 ,
                              face = "bold",
                              vjust = 1),
    axis.line = element_line(colour = "black",
                             size = base_size / 15,
                             lineend = "round"),
    axis.ticks = element_line(size = base_size / 15,
                              lineend = "round",
                              colour = "black"),
    axis.title.x=element_blank()



  ), class = c("theme", "gg"))

}
