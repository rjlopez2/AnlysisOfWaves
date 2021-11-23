#' add_Treatment_factor_func
#'
#' @param my_dataset Dataset without not `Treatment` factor defined.
#'
#' @return the same dataset with the factor `Treatment`.
#' @importFrom  utils tail
#' @export
#'
#' @examples # no Example jet provide
add_Treatment_factor_func <- function(my_dataset){

  my_dataset %>%
    group_by(.data$Date, .data$Experiment) %>%
    nest() %>%
    # head %>%
    mutate(data = purrr::map(.data$data, ~ (.x %>% mutate(Treatment = case_when( "cAMP" %in% .data$Condition ~ "cAMP",
                                                                                 "Fab" %in% .data$Condition ~ "Fab",
                                                                                 # "Fab-cAMP" %in% .data$Condition ~ "Fab-cAMP", # this is not working because previos (Fab "alone") condition match ths one too. need a clever aproach.
                                                                                 "Vehicle" %in% .data$Condition ~ "Vehicle",
                                                                                 TRUE ~ "no_defined")) %>%

                                              relocate(.data$Treatment, .before = .data$Condition)))) %>%
    # pluck("data", 4) %>%
    unnest(cols = .data$data) %>%
    ungroup()

}
