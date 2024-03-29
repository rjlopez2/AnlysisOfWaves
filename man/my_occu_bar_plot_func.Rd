% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_occu_bar_plot_func.R
\name{my_occu_bar_plot_func}
\alias{my_occu_bar_plot_func}
\title{my_occu_bar_plot_func}
\usage{
my_occu_bar_plot_func(
  my_dataset,
  my_var = "value",
  reffer_wave_thres = 10,
  line_size = 1,
  ...
)
}
\arguments{
\item{my_dataset}{A dataframe with raw data of waves.}

\item{my_var}{A character string of length 1. The name of the variable you want to make the barplot. This should be wave occurrence or something similar. Default to \code{"value"}.}

\item{reffer_wave_thres}{A double. Reference value in seconds indicating the threshold for the calculus of cumulative waves occurrence. Default to 10.}

\item{line_size}{Integer. the size of the lines to display. Default to 1.}

\item{...}{additional parameter to be passed to the aesthetic function \code{pptx_presentation_theme_func()}}
}
\value{
A ggplot object with barplot of waves occurrence in the different animals groups.
}
\description{
bar plot of waves occurrence
}
\examples{
# The example is still missing...
}
