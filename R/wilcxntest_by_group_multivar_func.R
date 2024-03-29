#' wilcxntest_by_group_multivar_func
#'
#' Wilcoxon test for multiples comparison
#'
#' @param my_dataset A cleaned dataframe.
#' @param my_var_set A character vector with the names of variables to test.
#' @param group_1 Character of one or more values. The name for the first grouping comparison variable. Default to `c("Treatment", "Condition")`.
#' @param group_2 Character. The name for the second grouping comparison variable. Default to `"Animal"`.
#' @param p_adj_met Character. One of the following methods p adjust methods: `c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"`. Defoult to `"BH`. for more info look at `p.adjust.methods`.
#' @param round_to Integer. A value to indicate number of decimals to used in final output. Default to 2.
#' @param ... Additional parameters passed to the function `rstatix::pairwise_wilcox_test()`.
#'
#' @return a datatable with the output of the Wilcoxon test
#' @export
#'
#' @examples # no example made jet.

wilcxntest_by_group_multivar_func <- function(my_dataset,
                                              my_var_set,
                                              group_1 = c("Treatment", "Condition"),
                                              group_2 = "Animal",
                                              p_adj_met = "BH",
                                              round_to = 2,
                                              ...){
  # group_1 <- syms(group_1)
  group_2 <- sym(group_2)


  result_table <- purrr::map_dfr(my_var_set,
                                 function(my_var){
                                   my_var <- sym(my_var)


                                   my_dataset %>%
                                     group_by(across(any_of(group_1))) %>%
                                     rstatix::pairwise_wilcox_test(formula = stats::formula(expr(!!my_var ~ !!group_2)),
                                                                   p.adjust.method = p_adj_met,
                                                                   detailed = TRUE,
                                                                   paired = FALSE,
                                                                   ...) # this was f**ing tricky to make it work -> check tidy evaluation and functions: "expr", "formula", "eval".
                                 })

  result_table <- result_table %>%
    dplyr::rename(Parameter = .data$.y.) %>%
    mutate(p.adjust.method = p_adj_met) %>%
    mutate(across(where(is.double), ~ round(.x, round_to)))

  return(result_table)

}
