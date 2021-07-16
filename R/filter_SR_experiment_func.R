#' Filter permeabilized cells data
#'
#' This function set to true or False experiment for analysis accordingly. If requires at least a dataset and a date. If other arguments are missing it will set by default all values (Experiments, Linescan, waves) to that one asiggned by the user in the given date.
#'
#' @param my_dataset A dataframe with raw data of waves.
#' @param my_date String. The date of the experiment you want to add or remove.
#' @param my_Experiment String. The Experiment with the cell ID indiacting permeabilization and coverslip. eg. `P1 CS1`.
#' @param my_Linescan String. The Linescan id. eg. `LS1`.
#' @param my_Wave_no String. The wave no id. eg. `W1`.
#' @param set_True Logical. You want to add or remove this experiment?. All experiments in the raw dataframe are by default set to `TRUE`.
#'
#' @return A datafranme with the column `For_Analysis` modified with the filtered experiments.
#' @export
#'
#' @examples # Examples are missing

filter_SR_experiment_func <- function(my_dataset,
                                      my_date,
                                      my_Experiment,
                                      my_Linescan,
                                      my_Wave_no,
                                      set_True = NULL){
  if(is.null(set_True) | missing(set_True)){
    stop("You may have forgotten to set the 'set_True' argument to 'True' or 'False'. Please indicate one logical value.")
  }
  if(!is.logical(set_True)){
    stop("The argumen 'set_True' must be a logical value eg. 'True' or 'False'. Please indicate one logical value.")
  }
  if(is.logical(set_True) & length(set_True) !=1){
    stop("The argumen 'set_True' value must be a single logical value eg. 'True' or 'False'. no multiples values. Please indicate ONLY one logical value.")
  }

  if(missing(my_dataset)){
    stop("No Dataset selected, you must select a Dataset", call. = T)
  }
  if(missing(my_date)){
    stop("No Date selected, you must select at least one Date", call. = T)
  }

  if(missing(my_Experiment)){ # This works fine

    my_Experiment <- my_dataset %>%
      filter(.data$Date %in% c(my_date)) %>%
      distinct(.data$Experiment) %>%
      unlist(recursive = T, use.names = F) %>%
      as.character()
  }

  #print(my_Experiment)

  if(missing(my_Linescan)){

    my_Linescan <- my_dataset %>%
      filter(.data$Date %in% c(my_date),
             .data$Experiment %in% c(!!!my_Experiment)) %>%
      distinct(.data$Linescan) %>%
      unlist(recursive = T, use.names = F) %>%
      as.character()

  }

  #print(my_Linescan)

  if(missing(my_Wave_no)){

    my_Wave_no <- my_dataset %>%
      filter(.data$Date %in% c(my_date),
             .data$Experiment %in% c(!!!my_Experiment),
             .data$Linescan %in% c(my_Linescan)) %>%
      distinct(.data$Wave_No) %>%
      unlist(recursive = T, use.names = F) %>%
      as.character()
  }

  #print(my_Wave_no)

  my_dataset <- transform(my_dataset,
                          For_Analysis = case_when(.data$Wave_No %in% c(my_Wave_no) &
                                                     .data$Linescan %in% c(my_Linescan) &
                                                     .data$Experiment %in% c(my_Experiment) &
                                                     .data$Date %in% c(my_date) ~ set_True,
                                                   T ~ For_Analysis))


  return(my_dataset)
}

