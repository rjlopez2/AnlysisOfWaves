#' make_multi_profile_func
#'
#' Make plot profile of multiples  linescan images. Note that the folder containing the images must follow this hierarchical structure: Animal -> Condition.
#'
#' @param my_root_img_dir string. The root directory containing the linescan image(s). files must end with the following suffix "Smth" and the extention ".tif".
#'
#' @return A dataframe. a rectangular data structure containing the varibales: Time (ms), Fluorescence (in AU), Animal (fct) and Condition (fct)
#' @export
#'
#' @examples # example is missing.
make_multi_profile_func <- function(my_root_img_dir){

  my_fig_list <- my_root_img_dir %>%
    list.files(pattern = "*Smth\\.tif", full.names = T, recursive = T)

  all_traces <- purrr::map_dfr(my_fig_list, function(trace){
    my_trace <- make_profile_func(trace)

    my_trace$Animal <- purrr::map_chr(trace, function(x){
      purrr::map_chr(str_split(trace, pattern = "/"), `[[`, 10)
    })

    my_trace$Condition <- purrr::map_chr(trace, function(x){
      purrr::map_chr(str_split(trace, pattern = "/"), `[[`, 11)
    })

    my_trace

  })

  all_traces$Animal <- forcats::as_factor(all_traces$Animal) %>%  forcats::fct_relevel("WT")
  # my_traces$Condition[my_traces$Condition == "Iso"] <- "ISO"
  all_traces$Condition <- forcats::as_factor(all_traces$Condition) %>% forcats::fct_relevel("Control", "Fab")

  all_traces

}
