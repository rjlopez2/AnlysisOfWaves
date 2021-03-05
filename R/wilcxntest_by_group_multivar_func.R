#' wilcxntest_by_group_multivar_func
#'
#' Wilcoxon test for multiples comparison
#'
#' @param my_dataset A cleaned dataframe.
#' @param my_var_set A character vector with the names of variables to test.
#' @param group_1 Character of one or more values. The name for the first grouping comparison variable. Default to `c("Treatment", "Condition")`.
#' @param group_2 Character. the name for the second grouping comparison variable. Default to `"Animal"`.
#'
#' @return a datatable with the output of the Wilcoxon test
#' @export
#'
#' @examples # no example made jet.
wilcxntest_by_group_multivar_func <- function(my_dataset, my_var_set,
                                              group_1 = c("Treatment", "Condition"),
                                              group_2 = "Animal"){

  result_table <- purrr::map_dfr(my_var_set, function(my_var){

    group_1 <- syms(group_1)# add multiples strings with sym(s)
    group_2 <- sym(group_2)
    my_var <- sym(my_var)

    my_dataset %>%
      group_by(!!!group_1) %>%
      rstatix::pairwise_wilcox_test(formula = stats::formula(expr(!!my_var ~ !!group_2)),
                                    p.adjust.method = "BH",
                                    detailed = TRUE,
                                    paired = FALSE) %>%  # this was f**ing tricky to make it work -> check tidy evaluation and fucntions: "expr", "formula", "eval".
      select(.data$Treatment, .data$Condition, everything()) %>%
      arrange(.data$Treatment, .data$Condition)
  })

  result_table

}
