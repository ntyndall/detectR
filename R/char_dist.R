#' @title Distribution atttributes
#'
#' @description A function that provides a wrapped around
#'  \code{charDist} to handle vectors of arguments supplied.
#'
#' @param args A character vector of arguments to be sent
#'  to determine the ICD.
#'
#' @return A matrix with 6 columns, one for each \code{dist_i}
#'  value of the ICD, and with the number of rows equal to the
#'  length of \code{args}.
#'
#' @export


char_dist <- function(args) {
  return(
    args %>%
      lapply(detectR::icdist) %>%
      purrr::flatten_dbl() %>%
      matrix(ncol = 6, byrow = TRUE)
  )
}
