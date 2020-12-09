
#' add_expe_and_counter_func
#'
#' Add couter for each experiment.
#'
#' @param my_dataset
#'
#' @return
#' @export
#'
#' @examples
add_expe_and_counter_func <- function(my_dataset){
  my_dataset <-  my_dataset %>%
    mutate(Experiment = str_sub(string = filename, 10, 15),
           Date_plus_experiment = str_sub(string = filename, 1, 15)) %>%
    add_count(Date_plus_experiment, name = "n_paired")

  return(my_dataset)
}
