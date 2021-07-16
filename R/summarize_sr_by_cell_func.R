#' Sumarize permeabilized cells wave data by cell
#'
#' This function aggregate (average) multiples waves to single cells level of the permeabilized cells dataset. It computes the cell aggregation to all the variables (numeric columns) in the dataset.
#'
#' @param dataset A dataframe with raw data of waves.
#' @param my_grouping_vars Character vector. A character vector of groups names assigned to perform the cell aggregation. Don't change at least you know what you are doing!
#'
#' @return A dataframe with cellular level data agregated, that is, a single point per cell.
#' @export
#'
#' @examples # the example is missing
summarize_sr_by_cell_func <- function(dataset,
                                      my_grouping_vars = c("Date",
                                                           "Animal_No",
                                                           "Animal",
                                                           "Experiment",
                                                           "Condition",
                                                           "Treatment")){

  dataset <- dataset %>%
    group_by(across(all_of(c(my_grouping_vars)))) %>%
    summarise(across(where(is.double),
                     ~ mean(.x, na.rm = TRUE)),
              .groups = "drop_last")
  return(dataset)
}
