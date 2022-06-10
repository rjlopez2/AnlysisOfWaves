#' plot_residuals_func
#'
#' This functions take a nested df containing a model information including the column `augment` and return the residuals for a given model. Use this function chained with the `extract_model_info()`.
#'
#' @param model_nsdf A nested df containing a model object and previously processed with the `extract_model_info()` function.
#' @param augmented_df The column name containing the df with the fit vs residuals values of the model. By defoult this columns in named `"augment"`.
#' @param faceted_by_1 A string. Grouping variables for visualization. Default to `Condition + Animal`.
#' @param faceted_by_2 A string. Aditional grouping variables for visualization. Default to `.`.
#'
#' @return A new colum called `res_vs_fit_plot` containing ggplot objets with the residual plots of the model.
#' @export
#'
#' @examples # No run
#'
plot_residuals_func <- function(model_nsdf,
                                augmented_df = "augment",
                                faceted_by_1 = "Condition  + Animal",
                                faceted_by_2 = "."){
  augmented_df <- sym(augmented_df)
  my_parameter_name <- sym(names(model_nsdf)[1])
  model_name <- sym(names(model_nsdf)[3])



  model_nsdf %>%
    group_by(model_name, .add = T) %>%
    mutate(res_vs_fit_plot = purrr::map(!!augmented_df, ~ {
      .x  %>%
        # ungroup %>%
        ggplot2::ggplot(aes(x = .fitted,
                   y = .resid,
                   color = Animal,
                   shape = Condition)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0,
                   color = "black",
                   linetype = "dashed",
                   alpha = 0.5) +
        # ggplot2::facet_grid(. ~ Condition  + Animal ) +
        ggplot2::facet_grid(stats::reformulate(faceted_by_1, faceted_by_2)) +
        ggplot2::scale_colour_manual(values = c("#666666",
                                                "#CC0000")) +
        ggplot2::scale_fill_manual(values = c("#666666",
                                              "#CC0000")) +
        # ggplot2::ggtitle(label = paste0("Parameter = ", !!my_parameter_name)) + # this work for parameter name
        # ggplot2::ggtitle(label = paste("Model = ", !!model_name,  sep = "_")) +
        ggplot2::ggtitle(label = paste(paste0("Parameter = ", !!my_parameter_name),
                                       paste0("Model = ", !!model_name), sep  = "\n")) +

        # ggplot2::ggtitle(label = paste0("Model = ", !!model_name)) +
        pptx_presentation_theme_func(base_size = 12) +
        ggplot2::theme(plot.title = element_text(size=12))

    }))

}
