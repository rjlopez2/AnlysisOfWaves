#' make_profile_func
#'
#' Create plot-profile from a linescan image.
#'
#' @param my_dir "A directory with a linescan image in tiff format.
#'
#' @return two vector: "Time" in ms and "Fluorescence" the signal in AU.
#' @export
#'
#' @examples # example missing.
#'
make_profile_func <- function(my_dir){
  my_trace <- my_dir %>%
    ijtiff::read_tif() %>%
    `[` ( , , 1, 1) %>%
    rowMeans() %>%
    dplyr::tibble(Time = 1:length(.) * 2 / 1000, # set the time in seconds
           Fluorescence = .)
}

