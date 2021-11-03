#' list_dirs_depth_n_func
#'
#' list directories at defined deep steps
#'
#' @param p string. target directory.
#' @param n integer. Number of levels to list directories.
#'
#' @return A vector string containing all directories found in the seraching path.
#' @export
#'
#' @examples # No exmaples proveided jet
list_dirs_depth_n_func <- function(p, n) {
  res <- list.dirs(p, recursive = FALSE)
  if (n > 1) {
    add <- list_dirs_depth_n_func(res, n-1)
    c(res, add)
  } else {
    res
  }
}
