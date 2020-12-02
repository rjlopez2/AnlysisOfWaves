#' Standard Error of the Mean (SEM)
#'
#' @param x A numeric vector.
#' @param na.rm should the NA values be removed?
#'
#' @return The SEM from a given vector
#' @export
#'
#' @examples
#'
#' x <- c(0:10)
#' sem_x <- sem(x)
#'
sem_func <- function(x, na.rm=TRUE) { # no implemented in the main function yet
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
