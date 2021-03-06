#' @title Get Max Token From List
#'
#' @description A function that wraps around \code{max_token} to
#'  vectorize the token scores.
#'
#' @param tokenVec A vector of tokens plus scores.
#'
#' @return A vector of numerical values representing the tokenzied
#'  scores.
#'
#' @export


max_from_list <- function(tokenVec) {
  return(
    tokenVec %>%
      lapply(detectR::max_token) %>%
      purrr::map('score') %>%
      purrr::flatten_dbl() %>%
      matrix(ncol = 1)
  )
}
