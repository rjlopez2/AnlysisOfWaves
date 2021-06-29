#' wilcxntest_by_group_multivar_func
#'
#' Wilcoxon test for multiples comparison
#'
#' @param my_dataset A cleaned dataframe.
#' @param my_var_set A character vector with the names of variables to test.
#' @param group_1 Character of one or more values. The name for the first grouping comparison variable. Default to `c("Treatment", "Condition")`.
#' @param group_2 Character. The name for the second grouping comparison variable. Default to `"Animal"`.
#' @param p_adj_met Character. One of the following methods p adjust methods: `c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"`. Defoult to `"BH`. for more info look at `p.adjust.methods`.
#'
#' @return a datatable with the output of the Wilcoxon test
#' @export
#'
#' @examples # no example made jet.
#'
# wilcxntest_by_group_multivar_func <- function(my_dataset, my_var_set,
#                                               group_1 = c("Treatment", "Condition"),
#                                               group_2 = "Animal"){
#
#   result_table <- purrr::map_dfr(my_var_set, function(my_var){
#
#     group_1 <- syms(group_1)# add multiples strings with sym(s)
#     group_2 <- sym(group_2)
#     my_var <- sym(my_var)
#
#     my_dataset %>%
#       group_by(across(any_of(!!!group_1))) %>%
#       rstatix::pairwise_wilcox_test(formula = stats::formula(expr(!!my_var ~ !!group_2)),
#                                     p.adjust.method = "BH",
#                                     detailed = TRUE,
#                                     paired = FALSE) %>%  # this was f**ing tricky to make it work -> check tidy evaluation and fucntions: "expr", "formula", "eval".
#       select(any_of(.data$Treatment), .data$Condition, everything()) %>%
#       arrange(any_of(.data$Treatment), .data$Condition)
#   })
#
#   result_table
#
# }

wilcxntest_by_group_multivar_func <- function(my_dataset,
                                              my_var_set,
                                              group_1 = c("Treatment", "Condition"),
                                              group_2 = "Animal",
                                              p_adj_met = "BH"){
  group_1 <- syms(group_1)
  group_2 <- sym(group_2)


  result_table <- purrr::map_dfr(my_var_set,
                                 function(my_var){
                                   my_var <- sym(my_var)


                                   my_dataset %>%
                                     group_by(!!!group_1) %>%
                                     rstatix::pairwise_wilcox_test(formula = stats::formula(expr(!!my_var ~ !!group_2)),
                                                                   p.adjust.method = p_adj_met,
                                                                   detailed = TRUE,
                                                                   paired = FALSE) # this was f**ing tricky to make it work -> check tidy evaluation and functions: "expr", "formula", "eval".
                                 })

  result_table <- result_table %>%
    dplyr::rename(Parameter = .data$.y.) %>%
    mutate(p.adjust.method = p_adj_met)

  return(result_table)

}
