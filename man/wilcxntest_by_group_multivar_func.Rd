% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wilcxntest_by_group_multivar_func.R
\name{wilcxntest_by_group_multivar_func}
\alias{wilcxntest_by_group_multivar_func}
\title{wilcxntest_by_group_multivar_func}
\usage{
wilcxntest_by_group_multivar_func(
  my_dataset,
  my_var_set,
  group_1 = c("Treatment", "Condition"),
  group_2 = "Animal",
  p_adj_met = "BH",
  round_to = 2,
  ...
)
}
\arguments{
\item{my_dataset}{A cleaned dataframe.}

\item{my_var_set}{A character vector with the names of variables to test.}

\item{group_1}{Character of one or more values. The name for the first grouping comparison variable. Default to \code{c("Treatment", "Condition")}.}

\item{group_2}{Character. The name for the second grouping comparison variable. Default to \code{"Animal"}.}

\item{p_adj_met}{Character. One of the following methods p adjust methods: \verb{c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"}. Defoult to \verb{"BH}. for more info look at \code{p.adjust.methods}.}

\item{round_to}{Integer. A value to indicate number of decimals to used in final output. Default to 2.}

\item{...}{Additional parameters passed to the function \code{rstatix::pairwise_wilcox_test()}.}
}
\value{
a datatable with the output of the Wilcoxon test
}
\description{
Wilcoxon test for multiples comparison
}
\examples{
# no example made jet.
}
