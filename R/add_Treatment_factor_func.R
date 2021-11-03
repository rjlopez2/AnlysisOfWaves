#' add_Treatment_factor_func
#'
#' @param my_freq_summarized_data Dataset without not `Treatment` factor defined.
#'
#' @return the same dataset with the factor `Treatment`.
#' @importFrom  utils tail
#' @export
#'
#' @examples # no Example jet provide
add_Treatment_factor_func <- function(my_freq_summarized_data){

  my_freq_summarized_data <- ungroup(my_freq_summarized_data)
  my_freq_summarized_data$Condition <- as.character(my_freq_summarized_data$Condition)
  my_freq_summarized_data$Experiment <- as.character(my_freq_summarized_data$Experiment)
  my_freq_summarized_data$Date <- as.character(my_freq_summarized_data$Date)
  # print(my_freq_summarized_data)
  my_dates <- my_freq_summarized_data %>%

    select(.data$Date) %>%
    distinct %>%
    purrr::flatten_chr()
  # print(my_dates)

  Single_day_exp <- lapply(my_dates, function(k){
    # print(k)
    Experiments_list <- my_freq_summarized_data %>%
      filter(.data$Date == k) %>%
      select(.data$Experiment) %>%
      distinct %>%
      purrr::flatten_chr()
    # print(Experiments_list)

    full_data <- lapply(Experiments_list, function(i){
      # print(i)
      my_data <- lapply(i, function(j){
        # print(j)
        # print(k)
        my_condition <- my_freq_summarized_data %>%
          filter(.data$Date == k,
                 .data$Experiment == j,
                 .data$Condition != "Control") %>%
          select(.data$Condition) %>%
          distinct() %>%
          purrr::flatten_chr() %>%
          tail(1)
        # print(tail(my_condition, 1)) #


        my_freq_summarized_data <- my_freq_summarized_data %>%
          filter(.data$Date == k,
                 .data$Experiment == i) %>%
          mutate(.data$Treatment == my_condition)
        #Condition = if_else(str_detect(Condition, "Control"), my_condition, my_condition))
        # Treatment = my_condition)

        my_freq_summarized_data

      })

      my_data <- my_data %>%
        do.call("rbind.data.frame", .)

      my_data

    })

    full_data

  })

  Single_day_exp <- Single_day_exp %>%
    purrr::flatten_df()

  Single_day_exp

}
