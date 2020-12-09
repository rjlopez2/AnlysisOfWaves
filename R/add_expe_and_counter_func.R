
#' add_expe_and_counter_func
#'
#' Add counter for each experiment to asses paired experiment.
#'
#' @param my_dataset A dataframe containing variables filename.
#'
#' @return the dataframe and a new column named "n_paired" with values 1 for non-paired experiment or 2 pro paired experiments.
#' @export
#'
#' @examples # The example is still missing...
add_expe_and_counter_func <- function(my_dataset){
  my_dataset <-  my_dataset %>%
    mutate(Experiment = str_sub(string = .data$filename, 10, 15),
           Date_plus_experiment = str_sub(string = .data$filename, 1, 15)) %>%
    add_count(.data$Date_plus_experiment, name = "n_paired")

  return(my_dataset)
}
