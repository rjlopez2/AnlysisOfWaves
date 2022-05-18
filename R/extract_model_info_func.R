#' extract_model_info_func
#'
#' Get information from a model object. This function is a wraper from the {broom} package to glance, tidy and augment the model object. Extract and display also different disgnostic tools for model: eg. LogLik, AIC, BIC.
#'
#' @param ns_df A nested dataframe containing a column with a model object named `model`.
#'
#' @return A nested dataframe with additional columns containing model information.
#' @export
#'
#' @examples # example is missing.
extract_model_info_func <- function(ns_df){
  my_parameter_name <- sym(names(ns_df)[1])
  ns_df %>%
    # pivot_longer(cols = ends_with("model"),
    #              names_to = "model_name",
    #              values_to = "model") %>%
    mutate(across(.data$model,
                  list(glance = ~ purrr::map(.x, ~ broom::glance(.x)),
                       tidy =  ~ purrr::map(.x, ~ broom::tidy(.x)),
                       augment =  ~ purrr::map(.x, ~ broom::augment(.x))),
                  # augment =  ~ map(.x, ~ (broom::augment(.x) %>%
                  #                           mutate(.student.resid = .resid / .sigma * sqrt(1 - .hat))))), # compute inside the augment column the studentized residuals
                  .names = "{.fn}")) %>%
    mutate(logLik = (.data$glance %>% purrr::map_dbl(~ .x$logLik * -2)),
           AIC = (.data$glance %>% purrr::map_dbl(~ .x$AIC)),
           BIC = (.data$glance %>% purrr::map_dbl(~ .x$BIC)),
           # p_val = (glance %>% map_dbl(~ .x$p.value)),
           # sigma = (glance %>% map_dbl(~ .x$sigma))) %>%
           p_val = purrr::map(.data$model,  ~(.x %>%
                                                car::Anova() %>%
                                                broom::tidy())))
}
