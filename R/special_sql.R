#' @title Special SQL
#'
#' @export


special_sql <- function(argument) {
  return(
    argument %>%
      strsplit("") %>%
      purrr::map(function(x) x %in% c("*", ";", "'")) %>%
      purrr::map(sum) %>%
      purrr::flatten_dbl() %>%
      as.matrix
  )
}
