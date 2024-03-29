% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_arrange_by_condition_func.R
\name{plot_arrange_by_condition_func}
\alias{plot_arrange_by_condition_func}
\title{Arrange multiples plots by condition}
\usage{
plot_arrange_by_condition_func(
  dataframe,
  var,
  my_conditions,
  type_of_plot = "superplot",
  ...
)
}
\arguments{
\item{dataframe}{A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.}

\item{var}{Character vector. List of variable(s) you want to plot.}

\item{my_conditions}{Character vector. List of conditions you want to plot, eg. \code{c("Fab", "cAMP")}.}

\item{type_of_plot}{"Character. What kind of plot would you like to make? Options are 1. A regular boxplot using: \code{"reg_boxplot"}, 2. A superplot using: \code{"superplot"}. Default to \code{"superplot"}.}

\item{...}{Additional parameters passed to the type of plot.}
}
\value{
a list of multiples ggplot objects.
}
\description{
This helper function make multiples plot base on the treatments provided, eg. Fab, cAMP and not by the parameter analyzed or your variable of interest.
}
\examples{
# the example is still missing.
}
