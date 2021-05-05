#' Standard Error of the Mean (SEM)
#'
#' @param x A numeric vector.
#' @param na.rm Boolean. Should the NA values be removed? defoult to FALSE.
#'
#' @return The SEM from a given vector
#' @export
#'
#' @examples
#' x <- c(0:10)
#' sem_x <- sem_func(x)
#'
sem_func <- function(x, na.rm=FALSE) {
  if (na.rm) x <- stats::na.omit(x)
  sqrt(stats::var(x)/length(x))
}
