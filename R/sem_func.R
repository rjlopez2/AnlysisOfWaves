sem_func <- function(x, na.rm=TRUE) { # no implemented in the main function yet
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
